package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.schedule.{CalculationPeriod, ScheduledPayoffs, Schedule}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.equity.Equity
import net.squantlib.model.bond.PriceableBond
import net.squantlib.model.fx.FX
import net.squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._


case class EquityMc1f(valuedate:Date, 
					  mcengine:Montecarlo1f, 
					  scheduledPayoffs:ScheduledPayoffs, 
					  equity:Equity,
					  defaultPaths:Int,
					  bondid:String) extends PricingModel {
  
	mcPaths = defaultPaths

	override def modelPaths(paths:Int) = modelPaths(paths, (p:List[Double]) => scheduledPayoffs.price(p))
	
	def modelPaths(paths:Int, pricer:List[Double] => List[Double]):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {errorOutput(bondid, "MC paths - cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, pricer)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { modelOutput("error", List("invalid mc dates")); errorOutput(bondid, "invalid mc dates"); List.empty}
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
	    errorOutput(bondid, s"MC calculation error vd ${equity.valuedate} ${errormsg}"); List.empty}
	}
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))
	
  override def triggerProbabilities:List[Double] = triggerProbabilities(mcPaths)
  
  def triggerProbabilities(paths:Int):List[Double] = getOrUpdateCache("TriggerProb"+paths, {
    val prices = EquityMc1f(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, equity, defaultPaths, bondid).mcPrice(paths)
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
	    (c.triggers.get(equity.id), ul.headOption) match{
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
	  
	override def priceInfo = {
	  var result = "mcdates\taverage/1000paths\n"
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, 1000, p => p)
	  result += (mcdates, mcpaths).zipped.map{case (d, p) => d.toString + "\t" + (p.sum / p.size)}.mkString("\n")
	  result
	}
	
	override val mcEngine = Some(mcengine)
}


object EquityMc1f {
	
	var defaultPaths = 50000
	
	def apply(market:Market, bond:PriceableBond, mcengine:Equity => Option[Montecarlo1f]):Option[EquityMc1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:Equity => Option[Montecarlo1f], 
	    paths:Int):Option[EquityMc1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    errorOutput(bond.id, "payoff not compatible with Equity1d model")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on Equity1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val equity = market.getEquity(variable).orNull
	  
	  if (equity == null) {
	    errorOutput(bond.id, "invalid Equity underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (equity.currency != bond.currency) {
	    errorOutput(bond.id, "quanto model not supported - " + variable)
	    return None}
	  
	  val mcmodel = mcengine(equity).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None}
	  
	  Some(EquityMc1f(valuedate, mcmodel, scheduledPayoffs, equity, paths, bond.id))
	}
}

object EquityQtoMc1f {
	
	var defaultPaths = 50000
	
	def apply(market:Market, bond:PriceableBond, mcengine:(Equity, FX) => Option[Montecarlo1f]):Option[EquityMc1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:(Equity, FX) => Option[Montecarlo1f], 
	    paths:Int):Option[EquityMc1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    errorOutput(bond.id, "payoff not compatible with Equity1d model")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on Equity1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val equity = market.getEquity(variable).orNull
	  
	  if (equity == null) {
	    errorOutput(bond.id, "invalid Equity underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (equity.currency == bond.currency) {
	    errorOutput(bond.id, "non-quanto model not supported - " + variable)
	    return None}
	  
	  val fx = market.getFX(bond.currency.code, equity.currency.code).orNull

	  if (fx == null) {
	    errorOutput(bond.id, "invalid fx underlying for quanto model - " + fx.id + " in market " + market.paramset)
	    return None}
	  
	  val mcmodel = mcengine(equity, fx).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None}
	  
	  Some(EquityMc1f(valuedate, mcmodel, scheduledPayoffs, equity, paths, bond.id))
	}
}










