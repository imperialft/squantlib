package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod}
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import squantlib.model.rates.DiscountCurve
import squantlib.pricing.mcengine.SwaptionFormula


case class OneTimeSwaptionModel(ipayoffs:Payoffs, ischedule:Schedule, valueDate:qlDate, optionDate:qlDate, maturity:qlDate, strike:Double, curve:DiscountCurve) extends PricingModel {
  
	assert (ipayoffs.size == ischedule.size, "assertion failed : payoffsize=" + ipayoffs.size + " vs schedule.size=" + ischedule.size)
	assert (ipayoffs.variables.size == 0, "assertion failed : variables=" + ipayoffs.variables.size)
	
	val swaptionPrice = {
	  val exp = (optionDate.serialNumber.toDouble - valueDate.serialNumber.toDouble) / 365.0
	  val mat = (maturity.serialNumber.toDouble - optionDate.serialNumber.toDouble) / 365.0
	  val fwdRate = curve.forwardRate(optionDate, maturity)
	  val vol = curve.volatility(optionDate, maturity).getOrElse(Double.NaN)
	  val discount = curve.zc(optionDate)
	  SwaptionFormula.price(exp, mat, fwdRate, strike, vol, discount,false)
	}
	
	def price:List[Double] = ipayoffs.price
	
	override val optionValue:Option[Double] = Some(-swaptionPrice)
	
	val periods:List[CalculationPeriod] = ischedule.toList
	
	val payoff:List[Payoff] = ipayoffs.toList
}


object OneTimeSwaptionModel {
	
	def apply(market:Market, bond:Bond):Option[PricingModel] = {
	  val (schedule, payoffs) = bond.livePayoffs(market.valuedate)
	  if (payoffs.variables.size != 0) { return None }
	  
	  val maturity = bond.maturity
	  
	  val nextPayment = bond.nextPayment.collect{case (d, _) => d}.orNull
	  if (nextPayment == null) {return None}
	  if (nextPayment == maturity) { return NoModel(market, bond) }
	  
	  val curve = bond.discountCurve.orNull
	  if (curve == null) {return None}
	  
	  val strike = bond.nextRateFrontier.getOrElse(Double.NaN)
	  if (strike isNaN) {return None}
	  
	  Some(OneTimeSwaptionModel(payoffs, schedule, market.valuedate, nextPayment, maturity, strike, curve))
	}
}

