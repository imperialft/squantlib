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

  
	def price(paths:Int):Double = {
	  val (mcdates, mcpaths) = model.generatePaths(eventYears, paths)
	  if (!mcdates.sameElements(eventYears)) { println("invalid mc dates"); Double.NaN}
	  
	  val pricearray = mcpaths.map(payoffs.price).transpose.map(_.sum)
	  (pricearray, coefficients).zipped.map(_ * _).sum
	}
		
}

object FXMontecarlo1f {
  
	def apply(market:CurveFactory, bond:Bond):Option[FXMontecarlo1f] = {
	
	  val (schedule, bondPayoffs) = bond.livePayoffSchedule(market.valuedate)
	
	  if (bondPayoffs.variables.size != 1) { println(bond.id + " : payoff not compatible with FX1d model"); return None}
	  
	  val variable = bondPayoffs.variables.head
	  val eventDates = schedule.eventDates
	  
	  val fixings:List[Option[Double]] = eventDates.map(d => if (d le market.valuedate) Fixings(variable, d).collect{case f => f._2} else None)
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
	  
	  val coefficients = bond.coefficients(market).orNull
	  if (coefficients == null) {println(bond.id + " : discount curve or daycounter not defined"); return None}
	  
	  val eventYears = bond.eventYears
	  
	  Some(new FXMontecarlo1f(market, mcmodel, payoffs, eventYears, coefficients, settings))
	}
}

