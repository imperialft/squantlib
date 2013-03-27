package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.ScheduledPayoffs
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import squantlib.model.rates.DiscountCurve


case class NoModel(scheduledPayoffs:ScheduledPayoffs) extends PricingModel {
  
	override def calculatePrice:List[Double] = scheduledPayoffs.price
	
}


object NoModel {
	
	def apply(market:Market, bond:Bond):Option[NoModel] = {
	  val scheduledPayoffs = bond.livePayoffs(market.valuedate) 
	  if (scheduledPayoffs.underlyings.size != 0) { return None }
	  Some(new NoModel(scheduledPayoffs))
	}
}

