package squantlib.pricing.model

import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import scala.collection.mutable.Queue
import squantlib.model.{Market, Bond}

trait PricingModel {
	
	var mcPaths:Int
	
	val periods:List[CalcPeriod]
	
	val payoff:List[Payoff]
	
	def schedule:Schedule = Schedule(periods)	

	def payoffs:Payoffs = Payoffs(payoff)

	protected def price:List[Double]
	
	def currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	var message:Queue[String] = Queue.empty

	def priceLegs:List[Double] = {
	  val result = price
	  
	  val pricelegs = (schedule.toList, result).zipped
	  def rnd(v:Double):Double = math.round(v * 1000000.0) / 1000000.0
	  
	  message += currenttime.toString
	  message += "Paths: " + mcPaths
	  message += "EventDate, PayDate, Amount"
	  pricelegs.map{case (d, p) => message += d.eventDate.shortDate + " " + d.paymentDate.shortDate + " " + rnd(p)}
	  
	  result
	}
	
	def discountedPriceLegs(curve:DiscountCurve):List[Double] = 
	  (priceLegs zip schedule.coefficients(curve)).map{case (p, c) => p * c}
	
	def discountedPrice(curve:DiscountCurve):Double = discountedPriceLegs(curve).reduceLeft {_ + _}
	
	
}
