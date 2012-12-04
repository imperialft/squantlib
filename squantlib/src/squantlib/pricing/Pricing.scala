package squantlib.pricing

import squantlib.model.CurveFactory
import squantlib.payoff.{Payoffs, Schedule}
import squantlib.pricing.model._
import squantlib.model.fx.FX
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings

/* Pricing procedure:
 * 1) Get product information (and pricing settings if any)
 * 2) Initialize market with appropriate discounting
 * 3) Initialize pricing model and generate montecarlo paths(not necessary for fixed coupon)
 * 4) Initialize cashflow model
 * 5) Insert montecarlo paths into cashflow model and take average
 * 6) Discount
 */

class FXMontecarlo1f( val market:CurveFactory, 
					  val model:Montecarlo1f, 
					  val payoffs:Payoffs, 
					  val eventYears:List[Double],
					  val coefficients:List[Double], 
					  val settings:JsonNode) {
  
	assert (payoffs.size == eventYears.size, "assertion failed : payoffsize=" + payoffs.size + " vs eventYears.size=" + eventYears.size)
	assert (payoffs.size == coefficients.size, "assertion failed : payoffsize=" + payoffs.size + " vs coefficients.size=" + coefficients.size)
	assert (payoffs.variables.size == 1, "assertion failed : variables=" + payoffs.variables.size)
	
	val valuedate = market.valuedate
	
	val (fixlist, mclist) = (eventYears, payoffs, coefficients).zipped.toList.partition{case (e, p, c) => ((e <= 0) || (p.variables.size == 0))}
	val (mcYears, mcPayoff, mcCoefficients) = mclist.unzip3
	val mcPayoffs = Payoffs(mcPayoff)
	val (fixYears, fixPayoff, fixCoefficients) = fixlist.unzip3
	val fixPayoffs = Payoffs(fixPayoff)
	
	def priceFixlegs:List[(Double, Double)] = fixYears.zip((fixPayoffs.map(_.price), fixCoefficients).zipped.map(_ * _))
	
	def priceMClegs(paths:Int):List[(Double, Double)] = {
	  val (mcdates, mcpaths) = model.generatePaths(mcYears, paths)
	  if (!mcdates.sameElements(mcYears)) { println("invalid mc dates"); Double.NaN}
	  
	  val mcpricearray = mcpaths.map(mcPayoffs.price).transpose.map(_.sum)
	  mcYears.zip((mcpricearray, mcCoefficients).zipped.map(_ * _).map(_ / paths.toDouble))
	}
	
	def price(paths:Int) = (priceFixlegs ++ priceMClegs(paths)).unzip._2.sum
}

object FXMontecarlo1f {
  
	def apply(market:CurveFactory, bond:Bond):Option[FXMontecarlo1f] = {
			
	  val valuedate = market.valuedate
	  
	  val (schedule, bondPayoffs) = bond.livePayoffSchedule(valuedate)
	  
	  if (bondPayoffs.variables.size != 1) { println(bond.id + " : payoff not compatible with FX1d model"); return None}
	  
	  val variable = bondPayoffs.variables.head
	  val eventDates = schedule.eventDates
	  
	  val fixings:List[Option[Double]] = eventDates.map(d => if (d le valuedate) Fixings(variable, d).collect{case f => f._2} else None)
	  val payoffs = bondPayoffs.applyFixing(fixings)
	  
	  val fx = market.getFX(variable).orNull
	  if (fx == null) {println("invalid fx underlying - " + payoffs.variables.head); return None}
	  
	  val settings = bond.settings.orNull
	  if (settings == null) {println(bond.id + " : bond price setting required"); return None}
	  
	  val modelname = bond.settings.get.parseJsonString("model")
	  if (modelname == null) {println(bond.id + " : model name not defined"); return None}
	  
	  val mcmodel:Montecarlo1f = (modelname match {
	    case "FXBlackScholes1f" => FXBlackScholes1f(fx)
	    case "FXzeroVol" => FXzeroVol1f(fx)
	    case _ => None
	  }).orNull
	  if (mcmodel == null) {println(bond.id + " : model name not found or model calibration error"); return None}
	  
	  val discountCurve = bond.discountCurve(market).orNull
	  if (discountCurve == null) {println(bond.id + " : discount curve couldn't be initialized - " + bond.db.currencyid); return None}
	  
	  val coefficients = schedule.coefficients(discountCurve)
	  if (coefficients == null) {println(bond.id + " : daycounter not defined"); return None}
	  
	  val eventYears = schedule.eventYears(valuedate)
	  for (i <- 0 to eventYears.size - 1) println(schedule(i) + " " + eventYears(i) + " " + payoffs(i))
	  
	  Some(new FXMontecarlo1f(market, mcmodel, payoffs, eventYears, coefficients, settings))
	}
}

