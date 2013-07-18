package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod, ScheduledPayoffs}
import squantlib.pricing.mcengine._
import squantlib.model.index.Index
import squantlib.model.Underlying
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import org.jquantlib.daycounters.Actual365Fixed


case class Forward(
    valuedate:qlDate, 
    scheduledPayoffs:ScheduledPayoffs,
    underlyings:List[Underlying]) extends PricingModel {
	
	val cachedPrice = scala.collection.mutable.WeakHashMap[String, List[Double]]()
	
	override def calculatePrice:List[Double] = scheduledPayoffs.map{case (d, p, c) => p.price(underlyings.map(_.forward(d.eventDate)))} (collection.breakOut)
	
	override def modelForward(paths:Int):List[Double] = modelPaths(paths).transpose.map(_.sum).map(_ / paths)
	
	override val priceType = "MODEL"
	  
	override val mcEngine = None
}


object Forward {
	
	def apply(market:Market, bond:Bond):Option[Forward] = {
	  val scheduledPayoffs = bond.livePayoffs(market.valuedate) 
	  val underlyings = bond.underlyings.map(u => Underlying(u, market))
	  if (underlyings.forall(_.isDefined)) Some(new Forward(market.valuedate, scheduledPayoffs, underlyings.map(_.get)))
	  else None
	}
}









