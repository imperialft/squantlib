package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.{Payoffs, Payoff}
import net.squantlib.schedule.{ScheduledPayoffs, Schedule, CalculationPeriod}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.fx.FX
import net.squantlib.model.bond.PriceableBond
import net.squantlib.util.JsonUtils._
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import org.codehaus.jackson.JsonNode
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._

case class FxMc1f(valuedate:Date, 
            mcengine:Montecarlo1f, 
            scheduledPayoffs:ScheduledPayoffs, 
            fx:FX,
            defaultPaths:Int,
            trigger:List[Option[Double]],
            frontierFunction:() => List[Option[Double]],
            parameterRepository:Any => Unit,
            bondid:String) extends PricingModel {
  
  mcPaths = defaultPaths
  val redemamt = scheduledPayoffs.bonusAmount.takeRight(trigger.size)

  override def modelPaths(paths:Int):List[List[Double]] = modelPaths(paths, (p:List[Double]) => scheduledPayoffs.price(p, trigger, redemamt))
  
  def modelPaths(paths:Int, pricing:List[Double] => List[Double]):List[List[Double]] = {
    val mcYears = scheduledPayoffs.eventDateYears(valuedate)
    val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, pricing)
    if (mcdates.sameElements(mcYears)) mcpaths
    else { errorOutput(bondid, "invalid mc dates"); List.empty}
  }
   
  def mcPrice(paths:Int):List[Double] = {
    try { 
      val mpaths = modelPaths(paths)
      if (mpaths.isEmpty) scheduledPayoffs.price
      else concatList(mpaths).map(_ / paths.toDouble)
    }
    catch {case e:Throwable => errorOutput(bondid, s"MC calculation error : vd ${fx.valuedate} " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
  }
  
  override def modelForward(paths:Int):List[Double] = concatList(modelPaths(paths)).map(_ / paths)
    
  override def calculatePrice:List[Double] = calculatePrice(mcPaths)
  
  def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))
  
  override def triggerProbabilities:List[Double] = triggerProbabilities(mcPaths)
  
  def triggerProbabilities(paths:Int):List[Double] = getOrUpdateCache("TriggerProb"+paths, {
    val maxdate = scheduledPayoffs.schedule.paymentDates.max
    val prices = FxMc1f(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, fx, defaultPaths, trigger, frontierFunction, parameterRepository, bondid).mcPrice(paths)
    (scheduledPayoffs, prices).zipped.map{case ((cp, _, _), price) => price * cp.dayCount}.toList
  })
  
  
  override def calibrate:FxMc1f = {
    val frontier = frontierFunction()
    parameterRepository(frontier)
    modelOutput("exercise_frontier", frontier.map(_.collect{case v => (v * 10000.0).round / 10000.0}.getOrElse(0)))
    val newmodel = FxMc1f(valuedate, mcengine, scheduledPayoffs, fx, mcPaths, frontier, frontierFunction, parameterRepository, bondid)
    newmodel.modelOutput = modelOutput
    newmodel
  }
  
  def binaryPathMtM(range:Double, discounts:List[Double]):List[Double] => List[Double] = (underlyingPrices:List[Double]) => {
    val prices = (scheduledPayoffs.price(underlyingPrices), scheduledPayoffs.schedule.dayCounts, discounts).zipped.map{case (p, dc, zc) => p * dc * zc}
    
    @tailrec def forwardSum(input:List[Double], result:List[Double]):List[Double]= input match {
      case Nil => result
      case h::t => forwardSum(t, (h + result.headOption.getOrElse(0.0)) :: result)
    }
    
    val underlyingFixings = scheduledPayoffs.fixingPrices(underlyingPrices)
    val remainingMtM = forwardSum(prices.tail.reverse, List(0.0)).zip(discounts).map{case (p, zc) => p / zc}
    //skip the "current" coupon
    (remainingMtM, underlyingFixings, scheduledPayoffs.calls).zipped.map{case (p, ul, c) =>
      (c.triggers.get(fx.id), ul.headOption) match{
        case (Some(t), Some(ull)) if ull >= t * (1.0 - range) && ull < t => p
        case _ => 0.0
      }
    }
  }
  
  override def binarySize(paths:Int, range:Double, curve:DiscountCurve):List[Double] = getOrUpdateCache("BinarySize"+paths+range, {
    val discounts = scheduledPayoffs.schedule.paymentDates.map(d => curve(d))
    val data = modelPaths(paths, binaryPathMtM(range, discounts))
    data.transpose.map(binaries => binaries.sum / binaries.filter(_ != 0.0).size).zip(scheduledPayoffs.calls).map{case (b, p) => 1.0 + p.bonus - b}
  })
  
  override val priceType = "MODEL"
    
  override val mcEngine = Some(mcengine)
    
}


object FxMc1f {
  
  var defaultPaths = 100000
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
      errorOutput(bond.id, "unsupported variable size for FXMC1 model " + scheduledPayoffs.underlyings.size)
      return None}
    
    val variable = scheduledPayoffs.underlyings.head
    
    val fx = market.getFX(variable).orNull
    
    if (fx == null) {
      errorOutput(bond.id, "invalid fx underlying for FXMC1 model - " + variable + " in market " + market.paramset)
      return None}
    
    if (fx.currencyDom != bond.currency) {
      errorOutput(bond.id, "quanto model not supported by FXMC1 model - " + variable)
      return None}
    
    val mcmodel = mcengine(fx).orNull
    
    if (mcmodel == null) {
      errorOutput(bond.id, "model name not found or model calibration error")
      return None}
    
    Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, frontierFunction(bond, frontierPths), paramRepository(bond), bond.id))
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
      errorOutput(bond.id, "unsupported variable size for FXMC1 model " + scheduledPayoffs.underlyings.size)
      return None}
    
    val variable = scheduledPayoffs.underlyings.head
    
    val fx = market.getFX(variable).orNull
    
    if (fx == null) {
      errorOutput(bond.id, "invalid fx underlying for FXMC1 model - " + variable + " in market " + market.paramset)
      return None}
    
    if (fx.currencyDom == bond.currency) {
      errorOutput(bond.id, "non-quanto model not supported by FXQtoMC1 model - " + variable)
      return None}
    
    val qtofx = market.getFX(bond.currency.code, fx.currencyDom.code).orNull

    if (qtofx == null) {
      errorOutput(bond.id, "invalid fx underlying for quanto model - " + qtofx.id + " in market " + market.paramset)
      return None}
    
    val mcmodel = mcengine(fx, qtofx).orNull
    
    if (mcmodel == null) {
      errorOutput(bond.id, "model name not found or model calibration error")
      return None}
    
    Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, FxMc1f.frontierFunction(bond, frontierPths), FxMc1f.paramRepository(bond), bond.id))
  }
}
