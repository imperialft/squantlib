package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod, ScheduledPayoffs}
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
					  scheduledPayoffs:ScheduledPayoffs, 
					  fx:FX,
					  defaultPaths:Int,
					  trigger:List[Option[Double]],
					  frontierFunction:() => List[Option[Double]],
					  parameterRepository:Any => Unit) extends PricingModel {
  
	mcPaths = defaultPaths
	
	val ischedule = scheduledPayoffs.schedule
	val ipayoffs = scheduledPayoffs.payoffs
	
	val fullSchedule = (ischedule, ipayoffs).zipped.toList
	val mcindex = fullSchedule.zipWithIndex.filter{case ((d, p), i) => (d.eventDate gt valuedate) && (p.variables.size == 1 || trigger(i).isDefined)}.map(_._2)
	val mcYears = Schedule(mcindex.map(i => ischedule(i)).toList).eventYears(valuedate)
	val mcMap = mcindex.zipWithIndex.toMap
	val legs = ischedule.size
	val redemamt = scheduledPayoffs.calls.bonus.takeRight(trigger.size).map(_ + 1.0)
	val redemrate = (redemamt, ischedule.dayCounts).zipped.map(_ / _)

	def generatePaths(paths:Int):List[List[Double]] = {
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
	  if (mcdates.sameElements(mcYears)) 
	    mcpaths.map(p => List.tabulate(legs)(i => if (mcMap contains i) p(mcMap(i)) else 0.0))
	  else { println("invalid mc dates"); List.empty}
	}
	 
	def mcPrice(paths:Int):List[Double] = {
	  try { generatePaths(paths).map(p => ipayoffs.price(p, trigger, redemrate)).transpose.map(_.sum / paths.toDouble) }
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
	  FXMontecarlo1f(valuedate, mcengine, scheduledPayoffs, fx, mcPaths, frontier, frontierFunction, parameterRepository)
	}
	
}


object FXMontecarlo1f {
	
	var defaultPaths = 100000
	
	def apply(market:Market, bond:Bond, mcengine:FX => Option[Montecarlo1f], triggers:List[Option[Double]]):Option[FXMontecarlo1f] = apply(market, bond, defaultPaths, mcengine, triggers)
	
	def apply(market:Market, bond:Bond, mcengine:FX => Option[Montecarlo1f]):Option[FXMontecarlo1f] = apply(market, bond, defaultPaths, mcengine)
  
	def apply(market:Market, bond:Bond, paths:Int, mcengine:FX => Option[Montecarlo1f]):Option[FXMontecarlo1f] = {
	  val trig = bond.getCalibrationCache[List[Option[Double]]]("FXMontecarlo1f") match {
	    case Some(t) => t
	    case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  } 
	  apply(market, bond, paths, mcengine, trig)
	}
	
	def apply(
	    market:Market, 
	    bond:Bond, 
	    paths:Int, 
	    mcengine:FX => Option[Montecarlo1f], 
	    triggers:List[Option[Double]]):Option[FXMontecarlo1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.variables.size != 1) { 
	    println(bond.id + " : payoff not compatible with FX1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.variables.head
	  
	  val fx = market.getFX(variable).orNull
	  
	  if (fx == null) {
	    println(bond.id + " : invalid fx underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (fx.currencyDom != bond.currency) {
	    println(bond.id + " : quanto model not supported - " + variable)
	    return None}
	  
	  val mcmodel = mcengine(fx).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None}
	  
	  val frontierFunction = () => bond.fxFrontiers(1.00, 0.003, 20, 10000).map(t => if (t.isEmpty) None else t.head)
	  val paramRepository = (obj:Any) => bond.calibrationCache.update("FXMontecarlo1f", obj)
	  
	  Some(FXMontecarlo1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, frontierFunction, paramRepository))
	}
}


//
//
//
////	val fullSchedule = (ischedule, ipayoffs).zipped.toList
//	val mcindex = scheduledPayoffs.filter{case ((d, p), i) => (d.eventDate gt valuedate) && (p.variables.size == 1 || trigger(i).isDefined)}.map(_._2)
//	val mcYears = Schedule(mcindex.map(i => ischedule(i)).toList).eventYears(valuedate)
//	val mcMap = mcindex.zipWithIndex.toMap
//	val legs = ischedule.size
//	val trigAmounts = ischedule.dayCounts.map(1.0 / _)
//
//	def generatePaths(paths:Int):List[List[Double]] = {
//	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
//	  if (mcdates.sameElements(mcYears)) 
//	    mcpaths.map(p => List.tabulate(legs)(i => if (mcMap contains i) p(mcMap(i)) else 0.0))
//	  else { println("invalid mc dates"); List.empty}
//	}
//	 
//	def mcPrice(paths:Int):List[Double] = {
//	  try { generatePaths(paths).map(p => ipayoffs.price(p, trigger, trigAmounts)).transpose.map(_.sum / paths.toDouble) }
//	  catch {case e => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
//	}
//	
//	def modelForward(paths:Int):List[Double] = generatePaths(paths).transpose.map(_.sum).map(_ / paths)
//	  
//	private val cachedPrice = scala.collection.mutable.WeakHashMap[String, List[Double]]()
//	
//	def price:List[Double] = price(mcPaths)
//	
//	def price(paths:Int):List[Double] = cachedPrice.getOrElseUpdate("PRICE", mcPrice(paths))
//	
//	val payoff:List[Payoff] = ipayoffs.toList
//	
//	val periods = ischedule.toList
//	
//	override def calibrate:FXMontecarlo1f = {
//	  val frontier = frontierFunction()
//	  parameterRepository(frontier)
//	  FXMontecarlo1f(valuedate, mcengine, ipayoffs, ischedule, fx, mcPaths, frontier, frontierFunction, parameterRepository)
//	}
