package squantlib.pricing.model

import squantlib.model.market.Market
import squantlib.schedule.payoff.Payoffs
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod, Schedule}
import squantlib.pricing.mcengine._
import squantlib.model.index.Index
import squantlib.model.asset.Underlying
import squantlib.model.bond.PriceableBond
import squantlib.util.JsonUtils._
import squantlib.model.rates.DiscountCurve
import squantlib.util.Date
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import org.codehaus.jackson.JsonNode


case class Forward(
    valuedate:Date, 
    scheduledPayoffs:ScheduledPayoffs,
    underlyings:List[Underlying]) extends PricingModel {
	
	override def calculatePrice:List[Double] = scheduledPayoffs.map{case (d, p, c) => p.price(underlyings.map(_.forward(d.eventDate)))} (collection.breakOut)
	
	override def modelForward(paths:Int):List[Double] = modelPaths(paths).transpose.map(_.sum).map(_ / paths)
	
	override val priceType = "MODEL"
	  
	override val mcEngine = None
}


object Forward {
	
	def apply(market:Market, bond:PriceableBond):Option[Forward] = {
	  val scheduledPayoffs = bond.livePayoffs(market.valuedate) 
	  val underlyings = bond.underlyings.map(u => Underlying(u, market))
	  if (underlyings.forall(_.isDefined)) Some(new Forward(market.valuedate, scheduledPayoffs, underlyings.map(_.get)))
	  else None
	}
}









