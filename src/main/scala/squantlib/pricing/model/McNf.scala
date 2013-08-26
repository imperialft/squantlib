package squantlib.pricing.model

import squantlib.model.Market
import squantlib.schedule.payoff.{Payoff, Payoffs}
import squantlib.schedule.{CalculationPeriod, ScheduledPayoffs, Schedule}
import squantlib.pricing.mcengine._
import squantlib.model.{Bond, Underlying}
import squantlib.model.fx.FX
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import org.jquantlib.daycounters.Actual365Fixed
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}


case class McNf(
    valuedate:qlDate, 
    mcengine:MontecarloNf, 
	scheduledPayoffs:ScheduledPayoffs, 
	underlyings:List[Underlying],
	defaultPaths:Int) extends PricingModel {
  
	mcPaths = defaultPaths
	val variables:List[String] = underlyings.map(_.id)

	override def modelPaths(paths:Int):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {println("MC paths : cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, p => scheduledPayoffs.price(p))
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
	}
	
	def mcPrice(paths:Int):List[Double] = {
	  try { 
	    val mpaths = modelPaths(paths)
	    if (mpaths.isEmpty) scheduledPayoffs.price
	    else mpaths.transpose.map(_.sum / paths.toDouble) }
	  catch {case e:Throwable => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
	}
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE", mcPrice(paths))
	
	override def modelForward(paths:Int):List[Double] = modelPaths(paths).transpose.map(_.sum).map(_ / paths)
	
	override val priceType = "MODEL"
	  
	override def generatePaths(paths:Int):List[List[Map[String, Double]]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {println("MC paths : cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, p => p)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
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
	    bond:Bond, 
	    mcengine:List[Underlying] => Option[MontecarloNf]
	    ):Option[McNf] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:Bond, 
	    mcengine:List[Underlying] => Option[MontecarloNf], 
	    paths:Int):Option[McNf] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  val variables:List[String] = bond.underlyings
	  
	  if (variables.size <= 1) { 
	    println(bond.id + " : payoff not compatible with EquityNd model - need more than 2 variables")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    println(bond.id + " : callability not supported on McNd model")
	    return None}
	  
	  val underlyings = variables.map(v => market.getUnderlying(v).orNull)
	  
	  if (underlyings.exists(ul => ul == null)) {
	    val nullvariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    println(bond.id + " : invalid underlying - " + nullvariables.mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  if (underlyings.exists(ul => ul.currency != bond.currency)) {
	    val qtovariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    println(bond.id + " : quanto model not supported - " + qtovariables.mkString(", "))
	    return None}
	  
	  val mcmodel = mcengine(underlyings).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None} 
	  
	  Some(McNf(valuedate, mcmodel, scheduledPayoffs, underlyings, paths))
	}
}


object McQtoNf {
	
	var defaultPaths = 50000
	
	def apply(market:Market, 
	    bond:Bond, 
	    mcengine:List[(Underlying, FX)] => Option[MontecarloNf]
	    ):Option[McNf] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:Bond, 
	    mcengine:List[(Underlying, FX)] => Option[MontecarloNf], 
	    paths:Int):Option[McNf] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  val variables:List[String] = bond.underlyings
	  
	  if (variables.size <= 1) { 
	    println(bond.id + " : payoff not compatible with EquityNd model - need more than 2 variables")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    println(bond.id + " : callability not supported on McNd model")
	    return None}
	  
	  val underlyings = variables.map(v => market.getUnderlying(v).orNull)
	  
	  if (underlyings.exists(ul => ul == null)) {
	    val nullvariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    println(bond.id + " : invalid underlying - " + nullvariables.mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  if (!underlyings.exists(ul => ul.currency != bond.currency)) {
	    val qtovariables = (variables, underlyings).zipped.withFilter{case (vv, uu) => uu == null}.map(_._1)
	    println(bond.id + " : non-quanto model not supported - " + qtovariables.mkString(", "))
	    return None}
	  
	  val fxs = underlyings.map(ul => market.getFX(bond.currency.code, ul.currency.code).orNull)

	  if (fxs.exists(_ == null)) {
	    println(bond.id + " : invalid fx underlying for quanto model - " + fxs.map(_.id).mkString(", ") + " in market " + market.paramset)
	    return None}
	  
	  val mcmodel = mcengine(underlyings zip fxs).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None} 
	  
	  Some(McNf(valuedate, mcmodel, scheduledPayoffs, underlyings, paths))
	}
}








