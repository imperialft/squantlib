package squantlib.pricing.model

import squantlib.model.Market
import squantlib.schedule.ScheduledPayoffs
import squantlib.model.bond.PriceableBond
import squantlib.model.rates.DiscountCurve


case class NoModel(scheduledPayoffs:ScheduledPayoffs) extends PricingModel {
  
	override def calculatePrice:List[Double] = scheduledPayoffs.price
	
	override val priceType = "MODEL"
}


object NoModel {
	
	def apply(market:Market, bond:PriceableBond):Option[NoModel] = {
	  val scheduledPayoffs = bond.livePayoffs(market.valuedate) 
	  if (scheduledPayoffs.underlyings.size != 0) { return None }
	  Some(new NoModel(scheduledPayoffs))
	}
}

