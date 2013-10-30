package squantlib.pricing.model

import squantlib.model.market.Market
import squantlib.schedule.ScheduledPayoffs
import squantlib.model.bond.{PriceableBond, YieldAnalysis}
import squantlib.util.Date
import squantlib.model.rates.DiscountCurve
import squantlib.pricing.numerical.SwaptionFormula


case class OneTimeSwaptionModel(
    scheduledPayoffs:ScheduledPayoffs, 
    valueDate:Date, 
    optionDate:Date, 
    maturity:Date, 
    strike:Double, 
    curve:DiscountCurve) extends PricingModel {
  
	val swaptionPrice = {
	  val exp = (optionDate.serialNumber.toDouble - valueDate.serialNumber.toDouble) / 365.0
	  val mat = (maturity.serialNumber.toDouble - optionDate.serialNumber.toDouble) / 365.0
	  val fwdRate = curve.forwardRate(optionDate, maturity)
	  val vol = curve.volatility(optionDate, maturity).getOrElse(Double.NaN)
	  val discount = curve.zc(optionDate)
	  SwaptionFormula.price(exp, mat, fwdRate, strike, vol, discount,false)
	}
	
	override def calculatePrice:List[Double] = scheduledPayoffs.price
	
	override val optionPrice:Option[Double] = Some(-swaptionPrice)
	
	override val priceType = "MODEL"
}

object OneTimeSwaptionModel {
	
	def apply(market:Market, bond:PriceableBond):Option[PricingModel] = {
	  val valuedate = market.valuedate
	  val scheduledPayoffs = bond.livePayoffs(valuedate)
	  if (scheduledPayoffs.underlyings.size != 0) { return None }
	  
	  val maturity = bond.scheduledMaturity
	  
	  val nextPayment = bond.nextPayment.collect{case (d, _) => d}.orNull
	  if (nextPayment == null) {return None}
	  if (nextPayment == maturity) { return NoModel(market, bond) }
	  
	  val curve = bond.discountCurve.orNull
	  if (curve == null) {return None}
	  
	  val newbond = new PriceableBond(bond.db, bond.scheduledPayoffs, bond.underlyings) with YieldAnalysis
	  newbond.market = market
	  
	  val strike = newbond.nextRateFrontier.getOrElse(Double.NaN)
	  if (strike.isNaN || strike.isInfinity) {return None}
	  
	  Some(OneTimeSwaptionModel(scheduledPayoffs, valuedate, nextPayment, maturity, strike, curve))
	}
}

