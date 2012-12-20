package squantlib.pricing.model

import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import scala.collection.mutable.Queue
import squantlib.model.{Market, Bond}
import org.jquantlib.time.{Date => qlDate}

trait PricingModel {
  
  val isPricedByLegs = true
	
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
	message += "EventDate, PayDate, Amount"
	pricelegs.map{case (d, p) => message += d.eventDate.shortDate.toString + " " + d.paymentDate.shortDate.toString + " " + rnd(p)}
	  
	result
  }

  def discountedPriceLegs(curve:Option[DiscountCurve]):List[Double] = curve match {
	case Some(c) => (priceLegs zip schedule.coefficients(c)).map{case (p, c) => p * c}
	case None => List.empty
  }
	
  def discountedPrice(curve:Option[DiscountCurve]):Option[Double] = Some(discountedPriceLegs(curve).sum)
  
}


