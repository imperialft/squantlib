package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod}
import squantlib.pricing.mcengine._
import squantlib.model.index.{SmoothIndex, DiscreteIndex, Index}
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import org.jquantlib.daycounters.Actual365Fixed


case class IndexMontecarlo1f(valuedate:qlDate, 
					  mcengine:Montecarlo1f, 
					  ipayoffs:Payoffs, 
					  ischedule:Schedule,
					  index:Index,
					  defaultPaths:Int,
					  trigger:List[Option[Double]]
					  ) extends PricingModel {
  
	assert (ipayoffs.size == ischedule.size, "assertion failed : payoffsize=" + ipayoffs.size + " vs schedule.size=" + ischedule.size)
	assert (ipayoffs.variables.size == 1, "assertion failed : variables=" + ipayoffs.variables.size)
	
	mcPaths = defaultPaths
	
	val fullSchedule = (ischedule, ipayoffs).zipped.toList
	
	val mcindex = fullSchedule.zipWithIndex.filter{
	  case ((d, p), i) => (d.eventDate gt valuedate) && (p.variables.size == 1 || trigger(i).isDefined)}.map(_._2)
	
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
}


object IndexMontecarlo1f {
	
	var defaultPaths = 100000
	
	def apply(market:Market, bond:Bond, mcengine:Index => Option[Montecarlo1f]):Option[IndexMontecarlo1f] = apply(market, bond, mcengine, defaultPaths)
	
	def apply(market:Market, bond:Bond, mcengine:Index => Option[Montecarlo1f], paths:Int):Option[IndexMontecarlo1f] = {
	  val valuedate = market.valuedate
	  
	  val (schedule, payoffs) = bond.livePayoffs(valuedate) match {case p => (p.schedule, p.payoffs)}
	  
	  if (payoffs.variables.size != 1) {
	    println(bond.id + " : payoff not compatible with IndexMC1d model")
	    return None}
	  
	  val variable = payoffs.variables.head
	  val index = market.getIndex(variable).orNull
	  
	  val triggers = bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
	  
	  if (index == null) {
	    println(bond.id + " : invalid index underlying - " + variable + " in market " + market.paramset)
	    return None}
	  
	  if (index.currency != bond.currency) {
	    println(bond.id + " : quanto model not supported - " + variable)
	    return None}
	  
	  if (bond.bermudan.exists(_ == true)) {
	    println(bond.id + " : index callable not supported")
	    return None}

	  val mcmodel:Montecarlo1f = mcengine(index).orNull
	  
	  if (mcmodel == null) {
	    println(bond.id + " : model name not found or model calibration error")
	    return None}
	  
	  Some(IndexMontecarlo1f(valuedate, mcmodel, payoffs, schedule, index, paths, triggers))
	}
}


