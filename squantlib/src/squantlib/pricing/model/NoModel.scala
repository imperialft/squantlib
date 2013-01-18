package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import squantlib.model.rates.DiscountCurve


case class NoModel(ipayoffs:Payoffs, ischedule:Schedule) extends PricingModel {
  
	assert (ipayoffs.size == ischedule.size, "assertion failed : payoffsize=" + ipayoffs.size + " vs schedule.size=" + ischedule.size)
	assert (ipayoffs.variables.size == 0, "assertion failed : variables=" + ipayoffs.variables.size)
	
	def price:List[Double] = ipayoffs.price
	
	val periods:List[CalcPeriod] = ischedule.toList
	
	val payoff:List[Payoff] = ipayoffs.toList
}


object NoModel {
	
	def apply(market:Market, bond:Bond):Option[NoModel] = {
	  val (schedule, payoffs) = bond.livePayoffs(market.valuedate)
	  if (payoffs.variables.size != 0) { return None }
	  Some(new NoModel(payoffs, schedule))
	}
}

