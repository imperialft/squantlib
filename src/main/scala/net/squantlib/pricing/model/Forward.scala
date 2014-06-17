package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.Payoffs
import net.squantlib.schedule.{ScheduledPayoffs, CalculationPeriod, Schedule}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.index.Index
import net.squantlib.model.asset.Underlying
import net.squantlib.model.bond.PriceableBond
import net.squantlib.util.JsonUtils._
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import org.codehaus.jackson.JsonNode


case class Forward(
    valuedate:Date, 
    scheduledPayoffs:ScheduledPayoffs,
    underlyings:List[Underlying]) extends PricingModel {
	
	override def calculatePrice:List[Double] = scheduledPayoffs.map{case (d, p, c) => p.price(underlyings.map(_.forward(d.eventDate)))} (collection.breakOut)
	
	override def modelForward(paths:Int):List[Double] = concatList(modelPaths(paths)).map(_ / paths)
	
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









