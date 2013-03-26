package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.ScheduledPayoffs
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import squantlib.model.rates.DiscountCurve

case class JGBMModel(bond:Bond, valueDate:qlDate) extends PricingModel {
  
	override val scheduledPayoffs:ScheduledPayoffs = bond.scheduledPayoffs
	
	override def calculatePrice:List[Double] = scheduledPayoffs.price
	
	var storedPrice:Option[Double] = None
	
	override def modelPrice(curve:DiscountCurve) = Some(discountedPriceLegs(curve).unzip._2.sum)
	
	override def price(curve:DiscountCurve):Option[Double] = {
	  if (valueDate ge bond.maturity) None
	  else if (valueDate lt bond.issueDate) bond.issuePrice.collect{case p => p / 100.0}
	  else {
	    if (storedPrice.isEmpty) {
	      val dbPrice = Fixings.byDate(bond.id, valueDate)
	      storedPrice = dbPrice.flatMap{case (d, p) => if(d == valueDate) Some(p / 100.0 + bond.accruedAmount.getOrElse(0.0)) else None} 
	    }
	    storedPrice
	  }
	}
}
	

object JGBMModel {
	
	def apply(market:Market, bond:Bond):Option[JGBMModel] = Some(JGBMModel(bond, market.valuedate))
	
}



