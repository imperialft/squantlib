package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod, ScheduledPayoffs}
import squantlib.pricing.mcengine._
import squantlib.model.index.Index
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import org.jquantlib.daycounters.Actual365Fixed


case class IndexMontecarlo1f(valuedate:qlDate, 
					  mcengine:Montecarlo1f, 
					  scheduledPayoffs:ScheduledPayoffs, 
					  index:Index,
					  defaultPaths:Int) extends PricingModel {
  
	mcPaths = defaultPaths

	override def modelPaths(paths:Int):List[List[Double]] = {
	  val mcYears = scheduledPayoffs.eventDateYears(valuedate)
	  if (mcYears.exists(_ < 0.0)) {println("MC paths : cannot compute past dates"); List.empty}
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
	}
	 
	def mcPrice(paths:Int):List[Double] = {
	  try { 
	    val mpaths = modelPaths(paths)
	    if (mpaths.isEmpty) scheduledPayoffs.price
	    else mpaths.map(p => scheduledPayoffs.price(p)).transpose.map(_.sum / paths.toDouble) }
	  catch {case e:Throwable => println("MC calculation error : " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
	}
	
	val cachedPrice = scala.collection.mutable.WeakHashMap[String, List[Double]]()
	
	override def calculatePrice:List[Double] = calculatePrice(mcPaths)
	
	def calculatePrice(paths:Int):List[Double] = cachedPrice.getOrElseUpdate("PRICE", mcPrice(paths))
	
	override def modelForward(paths:Int):List[Double] = modelPaths(paths).transpose.map(_.sum).map(_ / paths)
	
	override val priceType = "MODEL"
	  
	override val mcEngine = Some(mcengine)
}


object IndexMontecarlo1f {
	
	var defaultPaths = 50000
	
	def apply(market:Market, bond:Bond, mcengine:Index => Option[Montecarlo1f]):Option[IndexMontecarlo1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(
	    market:Market, 
	    bond:Bond, 
	    mcengine:Index => Option[Montecarlo1f], 
	    paths:Int):Option[IndexMontecarlo1f] = {
	  
	  val valuedate = market.valuedate
	  
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  
	  if (scheduledPayoffs.underlyings.size != 1) { 
	    println(bond.id + " : payoff not compatible with Index1d model")
	    return None}
	  
	  if (scheduledPayoffs.calls.isBermuda) { 
	    println(bond.id + " : callability not supported on Index1d model")
	    return None}
	  
	  val variable = scheduledPayoffs.underlyings.head
	  
	  val index = market.getIndex(variable).orNull
	  
	  if (index == null) {
	    println(bond.id + " : invalid index underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (index.currency != bond.currency) {
	    println(bond.id + " : quanto model not supported - " + variable)
	    return None}
	  
	  val mcmodel = mcengine(index).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None}
	  
	  Some(IndexMontecarlo1f(valuedate, mcmodel, scheduledPayoffs, index, paths))
	}
}









