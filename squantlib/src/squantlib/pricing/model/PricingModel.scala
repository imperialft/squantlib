package squantlib.pricing.model

import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod, ScheduledPayoffs}
import squantlib.model.rates.DiscountCurve
import scala.collection.mutable.Queue
import squantlib.model.{Market, Bond}
import org.jquantlib.time.{Date => qlDate}

trait PricingModel {
  
  /*	
   * Number of Montecarlo paths
   */
  var mcPaths:Int = 100000

  /*	
   * Target payoffs
   */
  val scheduledPayoffs:ScheduledPayoffs
  
  def schedule:Schedule = scheduledPayoffs.schedule
	
  def payoffs:Payoffs = scheduledPayoffs.payoffs
  
  /*	
   * Pricing function to be overridden. Result is annual rate without discount.
   */
  def price:List[Double]

  /*	
   * Returns forward value of each coupon. Result is annual rate without discount.
   */
  def forwardLegs:List[(CalculationPeriod, Double)] = schedule.toList zip price

  /*	
   * Returns price per payment legs.
   */
  def discountedPriceLegs(curve:DiscountCurve):List[(CalculationPeriod, Double)] = (price zip schedule).map{case (p, c) => (c, p * c.coefficient(curve))}
	
  /*
   * Returns present value of the future cashflow.
   * Override this function if the price is different from model calculated price. (eg. published price)
   */
  def discountedPrice(curve:DiscountCurve):Option[Double] = {
    val result = discountedPriceLegs(curve).unzip._2.sum
    if (result.isNaN) None else Some(result)
  }
  
  /*
   * Returns the model after pre-calibration.
   */
  def calibrate:PricingModel = this

  /*
   * Returns callable option price. Override if the product is callable.
   */
  val optionPrice:Option[Double] = None
  
  def getModel[T<:PricingModel]:Option[T] = this match {
    case m:T => Some(m)
    case _ => None
  }
}


