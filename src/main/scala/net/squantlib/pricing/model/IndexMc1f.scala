package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.{CalculationPeriod, Schedule, ScheduledPayoffs}
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.index.Index
import net.squantlib.model.bond.PriceableBond
import net.squantlib.model.fx.FX
import net.squantlib.util.Date
import net.squantlib.util.JsonUtils._
import net.squantlib.model.rates.DiscountCurve
import org.codehaus.jackson.JsonNode
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._

case class IndexMc1f(valuedate:Date, 
					  mcengine:Montecarlo1f, 
					  scheduledPayoffs:ScheduledPayoffs, 
					  index:Index,
					  defaultPaths:Int,
					  bondid:String) extends PricingModel {
  
	mcPaths = defaultPaths

	override def modelPaths(paths:Int):List[List[Double]] = modelPaths(paths, (p:List[Double]) => scheduledPayoffs.price(p))
	
	def modelPaths(paths:Int, pricing:List[Double] => List[Double]):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {errorOutput(bondid, "MC paths : cannot compute past dates"); List.empty}
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
    catch {case e:Throwable => 
      val errormsg = e.getStackTrace.mkString(sys.props("line.separator"))
      modelOutput("error", List(errormsg))
	    errorOutput(bondid, s"MC calculation error : ${bondid} vd ${index.valuedate} ${errormsg}")
	    List.empty}
  }
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))

  override def triggerProbabilities:List[Double] = triggerProbabilities(mcPaths)
  
  def triggerProbabilities(paths:Int):List[Double] = getOrUpdateCache("TriggerProb"+paths, {
    val maxdate = scheduledPayoffs.schedule.paymentDates.max
    val prices = IndexMc1f(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, index, defaultPaths, bondid).mcPrice(paths)
    (scheduledPayoffs, prices).zipped.map{case ((cp, _, _), price) => price * cp.dayCount}.toList
  })
	
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
      (c.triggers.get(index.id), ul.headOption) match{
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
  
	override def modelForward(paths:Int):List[Double] = concatList(modelPaths(paths)).map(_ / paths)
	
	override val priceType = "MODEL"
	  
	override val mcEngine = Some(mcengine)
}


object IndexMc1f {
	
	var defaultPaths = 200000
	
	def apply(market:Market, bond:PriceableBond, mcengine:Index => Option[Montecarlo1f]):Option[IndexMc1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:Index => Option[Montecarlo1f], 
	    paths:Int):Option[IndexMc1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    errorOutput(bond.id, "payoff not compatible with Index1d model")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on Index1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val index = market.getIndex(variable).orNull
	  
	  if (index == null) {
	    errorOutput(bond.id, "invalid index underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (index.currency != bond.currency) {
	    errorOutput(bond.id, "quanto product not supported by this model IndexMc1f - " + variable)
	    return None}
	  
	  val mcmodel = mcengine(index).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None}
	  
	  Some(IndexMc1f(valuedate, mcmodel, scheduledPayoffs, index, paths, bond.id))
	}
}

object IndexQtoMc1f {
	
	var defaultPaths = 200000
	
	def apply(market:Market, bond:PriceableBond, mcengine:(Index, FX) => Option[Montecarlo1f]):Option[IndexMc1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:(Index, FX) => Option[Montecarlo1f], 
	    paths:Int):Option[IndexMc1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    errorOutput(bond.id, "payoff not compatible with Index1d model")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on Index1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val index = market.getIndex(variable).orNull
	  
	  if (index == null) {
	    errorOutput(bond.id, "invalid index underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (index.currency == bond.currency) {
	    errorOutput(bond.id, "non-quanto products not supported by this model IndexQtoMc1f - " + variable)
	    return None}
	  
	  val fx = market.getFX(bond.currency.code, index.currency.code).orNull

	  if (fx == null) {
	    errorOutput(bond.id, "invalid fx underlying for quanto model - FX " + (if (fx == null) "null" else fx.id) + " in market " + (if (market == null) "null" else market.paramset))
	    return None}
	  
	  val mcmodel = mcengine(index, fx).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None}
	  
	  Some(IndexMc1f(valuedate, mcmodel, scheduledPayoffs, index, paths, bond.id))
	}
}







