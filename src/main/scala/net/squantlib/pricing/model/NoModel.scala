package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.ScheduledPayoffs
import net.squantlib.model.bond.PriceableBond
import net.squantlib.model.rates.DiscountCurve


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

