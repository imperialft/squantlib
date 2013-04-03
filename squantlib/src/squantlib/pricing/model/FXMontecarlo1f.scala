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

	override def modelPaths(paths:Int):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
	}
	 
	def mcPrice(paths:Int):List[Double] = {
	  val redemamt = scheduledPayoffs.bonusAmount.takeRight(trigger.size)
	  try { modelPaths(paths).map(p => scheduledPayoffs.price(p, trigger, redemamt)).transpose.map(_.sum / paths.toDouble) }
	  catch {case e:Throwable => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
	}
	
	override def modelForward(paths:Int):List[Double] = modelPaths(paths).transpose.map(_.sum).map(_ / paths)
	  
	val cachedPrice = scala.collection.mutable.WeakHashMap[String, List[Double]]()
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = cachedPrice.getOrElseUpdate("PRICE", mcPrice(paths))
	
	override def calibrate:FXMontecarlo1f = {
	  val frontier = frontierFunction()
	  parameterRepository(frontier)  
	  FXMontecarlo1f(valuedate, mcengine, scheduledPayoffs, fx, mcPaths, frontier, frontierFunction, parameterRepository)
	}
	
}


object FXMontecarlo1f {
	
	var defaultPaths = 100000
	var frontierPaths = 10000
	
	def apply(market:Market, bond:Bond, mcengine:FX => Option[Montecarlo1f]):Option[FXMontecarlo1f] = apply(market, bond, mcengine, defaultPaths)
  
	def apply(market:Market, bond:Bond, mcengine:FX => Option[Montecarlo1f], triggers:List[Option[Double]]):Option[FXMontecarlo1f] = apply(market, bond, mcengine, defaultPaths, triggers)
	
	def apply(market:Market, bond:Bond, mcengine:FX => Option[Montecarlo1f], paths:Int):Option[FXMontecarlo1f] = {
	  val trig = bond.getCalibrationCache("FXMontecarlo1f") match {
	    case Some(t:List[Any]) => t.map{
	      case Some(v:Double) => Some(v)
	      case _ => None
	    }.toList
	    case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  } 
	  apply(market, bond, mcengine, paths, trig)
	}
	
	def apply(
	    market:Market, 
	    bond:Bond, 
	    mcengine:FX => Option[Montecarlo1f], 
	    paths:Int, 
	    triggers:List[Option[Double]]):Option[FXMontecarlo1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    println(bond.id + " : unsupported variable size for FXMC1 model " + scheduledPayoffs.underlyings.size)
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val fx = market.getFX(variable).orNull
	  
	  if (fx == null) {
	    println(bond.id + " : invalid fx underlying for FXMC1 model - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (fx.currencyDom != bond.currency) {
	    println(bond.id + " : quanto model not supported by FXMC1 model - " + variable)
	    return None}
	  
	  val mcmodel = mcengine(fx).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None}
	  
	  val frontierFunction = () => bond.fxFrontiers(1.00, 0.003, 20, frontierPaths).map(t => if (t.isEmpty) None else t.head)
	  
	  val paramRepository = (obj:Any) => bond.calibrationCache.update("FXMontecarlo1f", obj)
	  
	  Some(FXMontecarlo1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, frontierFunction, paramRepository))
	}
}

