package squantlib.pricing.model

import squantlib.payoff.{Payoff, Payoffs, Schedule, CalculationPeriod, ScheduledPayoffs}
import squantlib.model.rates.DiscountCurve

trait PricingModel {
  
  /*	
   * Number of Montecarlo paths
   */
  var mcPaths:Int = 100000
  
  /*	
   * Returns montecarlo paths
   */
  def modelPaths(paths:Int = mcPaths):List[List[Double]] = List.empty
  
  /*	
   * Returns forward underlyings
   */
  def modelForward(paths:Int = mcPaths):List[Double] = List.empty
  

  /*	
   * Target payoffs
   */
  val scheduledPayoffs:ScheduledPayoffs
  
  /*	
   * Pricing function to be overridden. Result is annual rate without discount.
   */
  def calculatePrice:List[Double]

  /*	
   * Returns forward value of each coupon. Result is annual rate without discount.
   */
  def forwardLegs:List[(CalculationPeriod, Double)] = scheduledPayoffs.schedule.toList zip calculatePrice

  /*	
   * Returns price per payment legs.
   */
  def discountedPriceLegs(curve:DiscountCurve):List[(CalculationPeriod, Double)] = forwardLegs.map{case (c, p) => (c, p * c.coefficient(curve))}
	
  /*
   * Returns present value of the future cashflow.
   * Override this function if the price is different from model calculated price.
   */
  def price(curve:DiscountCurve):Option[Double] = {
    val result = discountedPriceLegs(curve).unzip._2.sum + optionPrice.getOrElse(0.0)
    if (result.isNaN) None else Some(result)
  }
  
  /*
   * Returns present value of the future cashflow.
   * Override this function if the price is different from model calculated price, and where discount curve is not necessary (eg. published price)
   */
  def price:Option[Double] = None
  
  /*
   * Price used for greeks calculation.
   */
  def modelPrice(curve:DiscountCurve):Option[Double] = price(curve)
  
  /*
   * Returns the model after pre-calibration.
   */
  def calibrate:PricingModel = this

  /*
   * Returns callable option price. Override if the product is callable.
   */
  val optionPrice:Option[Double] = None
  
}


