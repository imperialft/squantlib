package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod}
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import squantlib.model.rates.DiscountCurve

case class JGBMModel(bond:Bond, valueDate:qlDate) extends PricingModel {
  
	override val isPricedByLegs = false
	
	override def price:List[Double] = List.empty
	
	override val periods:List[CalculationPeriod] = List.empty
	
	override val payoff:List[Payoff] = List.empty
	
	var storedPrice:Option[Double] = None
	
	override def discountedPrice(curve:Option[DiscountCurve]):Option[Double] = {
	  if (valueDate ge bond.maturity) None
	  else {
	    if (storedPrice.isEmpty) {
	      val dbPrice = Fixings.byDate(bond.id, valueDate)
	      storedPrice = dbPrice.flatMap{case (d, p) => if(d == valueDate) Some(p) else None} 
	    }
	    storedPrice
	  }
	}
}
	

object JGBMModel {
	
	def apply(market:Market, bond:Bond):Option[JGBMModel] = Some(JGBMModel(bond, market.valuedate))
	
}



