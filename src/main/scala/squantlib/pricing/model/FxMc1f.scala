package squantlib.pricing.model

import squantlib.model.market.Market
import squantlib.schedule.payoff.{Payoffs, Payoff}
import squantlib.schedule.{ScheduledPayoffs, Schedule, CalculationPeriod}
import squantlib.pricing.mcengine._
import squantlib.model.fx.FX
import squantlib.model.bond.PriceableBond
import squantlib.util.JsonUtils._
import squantlib.model.rates.DiscountCurve
import squantlib.util.Date
import org.codehaus.jackson.JsonNode
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec

case class FxMc1f(valuedate:Date, 
					  mcengine:Montecarlo1f, 
					  scheduledPayoffs:ScheduledPayoffs, 
					  fx:FX,
					  defaultPaths:Int,
					  trigger:List[Option[Double]],
					  frontierFunction:() => List[Option[Double]],
					  parameterRepository:Any => Unit) extends PricingModel {
  
	mcPaths = defaultPaths
	val redemamt = scheduledPayoffs.bonusAmount.takeRight(trigger.size)

	override def modelPaths(paths:Int):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, p => scheduledPayoffs.price(p, trigger, redemamt))
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
	}
	 
	def mcPrice(paths:Int):List[Double] = {
	  try { 
	    val mpaths = modelPaths(paths)
	    if (mpaths.isEmpty) scheduledPayoffs.price
	    else {
//	      mpaths.transpose.map(_.sum).map(_ / paths.toDouble) 
	      concatList(mpaths).map(_ / paths.toDouble)
	    }
	  }
	  catch {case e:Throwable => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
	}
	
	override def modelForward(paths:Int):List[Double] = concatList(modelPaths(paths)).map(_ / paths)
	  
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))
	
  override def triggerProbabilities:List[Double] = triggerProbabilities(mcPaths)
  
  def triggerProbabilities(paths:Int):List[Double] = getOrUpdateCache("TriggerProb"+paths, {
    val maxdate = scheduledPayoffs.schedule.paymentDates.max
    val prices = FxMc1f(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, fx, defaultPaths, trigger, frontierFunction, parameterRepository).mcPrice(paths)
    (scheduledPayoffs, prices).zipped.map{case ((cp, _, _), price) => price * cp.dayCount}.toList
  })
	
	override def calibrate:FxMc1f = {
	  println("calibrate model")
	  val frontier = frontierFunction()
	  parameterRepository(frontier)
    modelOutput("exercise_frontier", frontier.map(_.getOrElse(null)))
	  val newmodel = FxMc1f(valuedate, mcengine, scheduledPayoffs, fx, mcPaths, frontier, frontierFunction, parameterRepository)
	  newmodel.modelOutput = modelOutput
	  newmodel
	}
	
	override val priceType = "MODEL"
	  
	override val mcEngine = Some(mcengine)
	  
}


object FxMc1f {
	
	var defaultPaths = 300000
	var frontierPaths = 15000
	
	def apply(market:Market, bond:PriceableBond, mcengine:FX => Option[Montecarlo1f]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths)
  
	def apply(market:Market, bond:PriceableBond, mcengine:FX => Option[Montecarlo1f], triggers:List[Option[Double]]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths, triggers)
	
	def apply(market:Market, bond:PriceableBond, mcengine:FX => Option[Montecarlo1f], paths:Int, frontierPths:Int):Option[FxMc1f] = {
	  val trig = bond.getCalibrationCache("FXMontecarlo1f") match {
	    case Some(t:List[Any]) => t.map{
	      case Some(v:Double) => Some(v)
	      case _ => None
	    }.toList
	    case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  } 
	  apply(market, bond, mcengine, paths, frontierPths, trig)
	}
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:FX => Option[Montecarlo1f], 
	    paths:Int, 
	    frontierPths:Int,
	    triggers:List[Option[Double]]):Option[FxMc1f] = {
	  
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
	  
	  Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, frontierFunction(bond, frontierPths), paramRepository(bond)))
	}
	
  def frontierFunction(bond:PriceableBond, frontierPths:Int) = () => bond.fxFrontiers(1.00, 0.003, 20, frontierPths).map(t => if (t.isEmpty) None else t.head)
	
  def paramRepository(bond:PriceableBond):Any => Unit = (obj:Any) => {
    bond.calibrationCache.update("FXMontecarlo1f", obj)
    obj match {
      case fs:List[d] =>
        (bond.livePayoffs, fs).zipped.map{
          case ((_, _, c), Some(f:Double)) => c.simulatedFrontier = Map(bond.underlyings.head -> f)
          case _ => {}}
      case _ => {}
    }
  }
	
}



object FxQtoMc1f {
	
	var defaultPaths = 300000
	var frontierPaths = 15000
	
	def apply(market:Market, bond:PriceableBond, mcengine:(FX, FX) => Option[Montecarlo1f]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths)
  
	def apply(market:Market, bond:PriceableBond, mcengine:(FX, FX) => Option[Montecarlo1f], triggers:List[Option[Double]]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths, triggers)
	
	def apply(market:Market, bond:PriceableBond, mcengine:(FX, FX) => Option[Montecarlo1f], paths:Int, frontierPths:Int):Option[FxMc1f] = {
	  val trig = bond.getCalibrationCache("FXMontecarlo1f") match {
	    case Some(t:List[Any]) => t.map{
	      case Some(v:Double) => Some(v)
	      case _ => None
	    }.toList
	    case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  } 
	  apply(market, bond, mcengine, paths, frontierPths, trig)
	}
	
	def apply( 
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:(FX, FX) => Option[Montecarlo1f], 
	    paths:Int, 
	    frontierPths:Int,
	    triggers:List[Option[Double]]):Option[FxMc1f] = {
	  
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
	  
	  if (fx.currencyDom == bond.currency) {
	    println(bond.id + " : non-quanto model not supported by FXQtoMC1 model - " + variable)
	    return None}
	  
	  val qtofx = market.getFX(bond.currency.code, fx.currencyDom.code).orNull

	  if (qtofx == null) {
	    println(bond.id + " : invalid fx underlying for quanto model - " + qtofx.id + " in market " + market.paramset)
	    return None}
	  
	  val mcmodel = mcengine(fx, qtofx).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None}
    
	  Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, FxMc1f.frontierFunction(bond, frontierPths), FxMc1f.paramRepository(bond)))
	}
}
