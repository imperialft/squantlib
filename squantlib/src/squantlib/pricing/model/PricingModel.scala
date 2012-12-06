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

	def price:List[Double]
	
	def currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	def priceWithDiscount(curve:DiscountCurve):Double = {
	  val result = price
	  val coeff = schedule.coefficients(curve)
	  
	  val pricelegs = (schedule.toList, result, coeff).zipped
	  def rnd(v:Double):Double = math.round(v * 1000000.0) / 1000000.0
	  
	  message += currenttime.toString
	  message += "Paths: " + mcPaths
	  message += "EventDate, PayDate, Amount, Coefficient, Price"
	  pricelegs.map{case (d, p, c) => message += d.eventDate.shortDate + " " + d.paymentDate.shortDate + " " + rnd(p) + " " + rnd(c) + " " + rnd(p * c)}
	  
	  (result zip coeff).map{case (p, c) => p * c}.sum
	}
	
	var message:Queue[String] = Queue.empty
	
}
