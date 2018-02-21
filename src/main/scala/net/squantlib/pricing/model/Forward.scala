package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.schedule.{CalculationPeriod, Schedule, ScheduledPayoffs}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.index.Index
import net.squantlib.model.asset.Underlying
import net.squantlib.model.bond.PriceableBond
import net.squantlib.util.JsonUtils._
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.schedule.call.Callability
import net.squantlib.util.Date

import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import org.codehaus.jackson.JsonNode

import scala.annotation.tailrec


case class Forward(
    valuedate:Date, 
    scheduledPayoffs:ScheduledPayoffs,
    underlyings:List[Underlying]) extends PricingModel {

	@tailrec private def calculatePriceRec(remainSchedule: List[(CalculationPeriod, Payoff, Callability)], acc: List[Double]): List[Double] = remainSchedule.headOption match {
		case Some((d, p, c)) =>
      val forwardPrices = p.eventDates(d).map(dd => underlyings.map(ul => (ul.id, ul.forward(dd))).toMap).toList
			calculatePriceRec(remainSchedule.tail, p.price(forwardPrices, acc) :: acc)
		case None => acc.reverse
	}

	override def calculatePrice:List[Double] = calculatePriceRec(scheduledPayoffs.toList, List.empty)

	//	override def calculatePrice:List[Double] = scheduledPayoffs.map{case (d, p, c) =>
//		p.price(underlyings.map(_.forward(d.eventDate)))
//	} (collection.breakOut)
	
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









