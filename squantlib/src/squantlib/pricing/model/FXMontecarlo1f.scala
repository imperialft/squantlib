package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod}
import squantlib.pricing.mcengine._
import squantlib.model.fx.FX
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import org.jquantlib.daycounters.Actual365Fixed


case class FXMontecarlo1f(valuedate:qlDate, 
					  mcengine:Montecarlo1f, 
					  ipayoffs:Payoffs, 
					  ischedule:Schedule,
					  fx:FX,
					  defaultPaths:Int,
					  trigger:List[Option[Double]],
					  frontierFunction:() => List[Option[Double]],
					  parameterRepository:Any => Unit) extends PricingModel {
  
	assert (ipayoffs.size == ischedule.size, "assertion failed : payoffsize=" + ipayoffs.size + " vs schedule.size=" + ischedule.size)
	assert (ipayoffs.variables.size == 1, "assertion failed : variables=" + ipayoffs.variables.size)
	
	mcPaths = defaultPaths
	
	val fullSchedule = (ischedule, ipayoffs).zipped.toList
	val mcindex = fullSchedule.zipWithIndex.filter{case ((d, p), i) => (d.eventDate gt valuedate) && (p.variables.size == 1 || trigger(i).isDefined)}.map(_._2)
	val mcYears = Schedule(mcindex.map(i => ischedule(i)).toList).eventYears(valuedate)
	val mcMap = mcindex.zipWithIndex.toMap
	val legs = ischedule.size
	val trigAmounts = ischedule.dayCounts.map(1.0 / _)

	def generatePaths(paths:Int):List[List[Double]] = {
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
	  if (mcdates.sameElements(mcYears)) 
	    mcpaths.map(p => List.tabulate(legs)(i => if (mcMap contains i) p(mcMap(i)) else 0.0))
	  else { println("invalid mc dates"); List.empty}
	}
	 
	def mcPrice(paths:Int):List[Double] = {
	  try { generatePaths(paths).map(p => ipayoffs.price(p, trigger, trigAmounts)).transpose.map(_.sum / paths.toDouble) }
	  catch {case e => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
	}
	
	def modelForward(paths:Int):List[Double] = generatePaths(paths).transpose.map(_.sum).map(_ / paths)
	  
	private val cachedPrice = scala.collection.mutable.WeakHashMap[String, List[Double]]()
	
	def price:List[Double] = price(mcPaths)
	
	def price(paths:Int):List[Double] = cachedPrice.getOrElseUpdate("PRICE", mcPrice(paths))
	
	val payoff:List[Payoff] = ipayoffs.toList
	
	val periods = ischedule.toList
	
	override def calibrate:FXMontecarlo1f = {
	  val frontier = frontierFunction()
	  parameterRepository(frontier)
	  FXMontecarlo1f(valuedate, mcengine, ipayoffs, ischedule, fx, mcPaths, frontier, frontierFunction, parameterRepository)
	}
	
}


object FXMontecarlo1f {
	
	var defaultPaths = 100000
	
	def apply(market:Market, bond:Bond, engineName:String, triggers:List[Option[Double]]):Option[FXMontecarlo1f] = apply(market, bond, defaultPaths, engineName, triggers)
	
	def apply(market:Market, bond:Bond, engineName:String):Option[FXMontecarlo1f] = apply(market, bond, defaultPaths, engineName)
  
	def apply(market:Market, bond:Bond, paths:Int, engineName:String):Option[FXMontecarlo1f] = {
	  val trig = bond.getCalibrationCache[List[Option[Double]]]("FXMontecarlo1f") match {
	    case Some(t) => t
	    case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  } 
	  apply(market, bond, paths, engineName, trig)
	}
	
	def apply(market:Market, bond:Bond, paths:Int, engineName:String, triggers:List[Option[Double]]):Option[FXMontecarlo1f] = {
	  val valuedate = market.valuedate
	  val (schedule, payoffs) = bond.livePayoffs(valuedate)
	  if (payoffs.variables.size != 1) { println(bond.id + " : payoff not compatible with FX1d model"); return None}
	  
	  val variable = payoffs.variables.head
	  val fx = market.getFX(variable).orNull
	  if (fx == null) {println(bond.id + " : invalid fx underlying - " + variable + " in market " + market.paramset); return None}
	  if (fx.currencyDom != bond.currency) {println(bond.id + " : quanto model not supported - " + variable); return None}

	  val mcmodel:Montecarlo1f = (engineName match {
	    case "FXzeroVol" => FXzeroVol1f(fx)
	    case "FXBlackScholes1f" => BlackScholes1f(fx)
	    case null => BlackScholes1f(fx) // default
	    case _ => None
	  }).orNull
	  if (mcmodel == null) {println(bond.id + " : model name not found or model calibration error"); return None}
	  
	  val frontierFunction = () => bond.fxFrontiers(1.00, 0.003, 20, 10000).map(t => if (t.isEmpty) None else t.head)
	  val paramRepository = (obj:Any) => bond.calibrationCache.update("FXMontecarlo1f", obj)
	  
	  Some(FXMontecarlo1f(valuedate, mcmodel, payoffs, schedule, fx, paths, triggers, frontierFunction, paramRepository))
	}
}


