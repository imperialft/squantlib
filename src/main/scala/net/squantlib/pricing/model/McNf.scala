package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.schedule.{CalculationPeriod, ScheduledPayoffs, Schedule}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.bond.PriceableBond
import net.squantlib.model.asset.Underlying
import net.squantlib.model.fx.FX
import net.squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._


case class McNf(
    valuedate:Date, 
    mcengine:MontecarloNf, 
    scheduledPayoffs:ScheduledPayoffs, 
    underlyings:List[Underlying],
    defaultPaths:Int,
    bondid:String) extends PricingModel {
  
  mcPaths = defaultPaths
  val variables:List[String] = underlyings.map(_.id)

  override def modelPaths(paths:Int):List[List[Double]] = modelPaths(paths, p => scheduledPayoffs.price(p))

  def modelPaths(paths:Int, pricer: List[Map[String,Double]] => List[Double]):List[List[Double]] = {
    val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {errorOutput(bondid, "MC paths : cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, pricer)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { errorOutput(bondid, "invalid mc dates"); List.empty}
	}
	
	def mcPrice(paths:Int):List[Double] = {
	  try { 
	    val mpaths = modelPaths(paths)
	    if (mpaths.isEmpty) scheduledPayoffs.price
	    else {
//	      mpaths.transpose.map(_.sum / paths.toDouble) 
        concatList(mpaths).map(_ / paths.toDouble)
	    }
	  }
    catch {case e:Throwable => 
      val errormsg = e.getStackTrace.mkString(sys.props("line.separator"))
      modelOutput("error", List(errormsg))
      errorOutput(bondid, s"MC calculation error vd ${underlyings.headOption.collect{case u => u.valuedate}.getOrElse("NA")} ${errormsg}"); List.empty}
  }
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))
	
  override def triggerProbabilities:List[Double] = triggerProbabilities(mcPaths)
  
  def triggerProbabilities(paths:Int):List[Double] = getOrUpdateCache("TriggerProb"+paths, {
    val maxdate = scheduledPayoffs.schedule.paymentDates.max
    val prices = McNf(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, underlyings, defaultPaths, bondid).mcPrice(paths)
    (scheduledPayoffs, prices).zipped.map{case ((cp, _, _), price) => price * cp.dayCount}.toList
  })
  
  def binaryPathMtM(range:Double, discounts:List[Double]):List[Map[String,Double]] => List[Double] = (underlyingPrices:List[Map[String, Double]]) => {
    val prices = (scheduledPayoffs.price(underlyingPrices), scheduledPayoffs.schedule.dayCounts, discounts).zipped.map{case (p, dc, zc) => p * dc * zc}
    
    @tailrec def forwardSum(input:List[Double], result:List[Double]):List[Double]= input match {
      case Nil => result
      case h::t => forwardSum(t, (h + result.headOption.getOrElse(0.0)) :: result)
    }
    
    val underlyingFixings:List[List[Map[String, Double]]] = scheduledPayoffs.fixingPrices(underlyingPrices)
    val remainingMtM = forwardSum(prices.tail.reverse, List(0.0)).zip(discounts).map{case (p, zc) => p / zc}
    //skip the "current" coupon
    (remainingMtM, underlyingFixings, scheduledPayoffs.calls).zipped.map{case (p, ul, c) =>
      ul.lastOption match{
        case Some(ull) if !c.isTriggered(ull) && c.triggers.keySet.subsetOf(ull.keySet) && c.triggers.forall{case (k, t) => ull(k) > t * (1.0 - range)} => p
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
	  
	override def generatePaths(paths:Int):List[List[Map[String, Double]]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {errorOutput(bondid, "MC paths : cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, p => p)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { errorOutput(bondid, "invalid mc dates"); List.empty}
	}
	   
	override def priceInfo = {
	  var result = "mcdates\taverage/1000paths\n"
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, 1000, p => p.map(q => 0.0))
	  result += (mcdates, mcpaths).zipped.map{case (d, p) => d.toString + "\t" + (p.sum / p.size)}.mkString("\n")
	  result
	}
	
	override val mcEngine = Some(mcengine)
}


object McNf {
	
	var defaultPaths = 50000
	
	def apply(market:Market, 
	    bond:PriceableBond, 
	    mcengine:List[Underlying] => Option[MontecarloNf]
	    ):Option[McNf] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:List[Underlying] => Option[MontecarloNf], 
	    paths:Int):Option[McNf] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  val variables:List[String] = bond.underlyings
	  
	  if (variables.size <= 1) { 
	    errorOutput(bond.id, "payoff not compatible with EquityNd model - need more than 2 variables")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on McNd model")
	    return None}
	  
	  val underlyings = variables.map(v => market.getUnderlying(v).orNull)
	  
	  if (underlyings.exists(ul => ul == null)) {
	    val nullvariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    errorOutput(bond.id, "invalid underlying - " + nullvariables.mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  if (underlyings.exists(ul => ul.currency != bond.currency)) {
	    val qtovariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    errorOutput(bond.id, "quanto model not supported - " + qtovariables.mkString(", "))
	    return None}
	  
	  val mcmodel = mcengine(underlyings).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None} 
	  
	  Some(McNf(valuedate, mcmodel, scheduledPayoffs, underlyings, paths, bond.id))
	}
}


object McNfQto {
	
	var defaultPaths = 50000
	
	def apply(market:Market, 
	    bond:PriceableBond, 
	    mcengine:(List[Underlying], List[Option[FX]]) => Option[MontecarloNf]
	    ):Option[McNf] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:PriceableBond, 
	    mcengine:(List[Underlying], List[Option[FX]]) => Option[MontecarloNf], 
	    paths:Int):Option[McNf] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  val variables:List[String] = bond.underlyings
	  
	  if (variables.size <= 1) { 
	    errorOutput(bond.id, "payoff not compatible with EquityNd model - need more than 2 variables")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    errorOutput(bond.id, "callability not supported on McNd model")
	    return None}
	  
	  val underlyings = variables.map(v => market.getUnderlying(v).orNull)
	  
	  if (underlyings.exists(ul => ul == null)) {
	    val nullvariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    errorOutput(bond.id, "invalid underlying - " + nullvariables.mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  if (!underlyings.exists(ul => ul.currency != bond.currency)) {
	    val qtovariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    errorOutput(bond.id, "non-quanto model not supported - " + qtovariables.mkString(", "))
	    return None}
	  
	  val fxs:List[Option[FX]] = underlyings.map(ul => 
	    if (ul.currency == bond.currency) None 
	    else Some(market.getFX(bond.currency.code, ul.currency.code).orNull))

	  if (fxs.exists(_ == Some(null))) {
	    errorOutput(bond.id, "invalid fx underlying for quanto model - " + underlyings.map(_.currency.code).mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  val mcmodel = mcengine(underlyings, fxs).orNull
	  
	  if (mcmodel == null) {
	    errorOutput(bond.id, "model name not found or model calibration error")
	    return None} 
	  
	  Some(McNf(valuedate, mcmodel, scheduledPayoffs, underlyings, paths, bond.id))
	}
}








