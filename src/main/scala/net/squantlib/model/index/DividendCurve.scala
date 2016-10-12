package net.squantlib.model.index

import net.squantlib.model.yieldparameter.YieldParameter
import net.squantlib.util.initializer.Currencies
import net.squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod}
import org.jquantlib.currencies.Currency
import net.squantlib.model.yieldparameter._


/**
 * Stores continuous dividend yield information.
 */
case class DividendCurve(rate:YieldParameter) extends YieldParameter {
  
  def value(d:Double) = rate(d)
  
  var valuedate = rate.valuedate
  
  val mindays = rate.mindays
  
  val maxdays = rate.maxdays
  
  override def shifted(v:(Double, Double) => Double):DividendCurve = DividendCurve(rate.shifted(v))
}

object DividendCurve{
  
  def buildCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter = 
    (values.keySet.size) match {
      case 1 => FlatVector(valuedate, values)
      case 2 => LinearNoExtrapolation(valuedate, values)
      case _ => SplineNoExtrapolation(valuedate, values, 2) } 
	
  def apply(valuedate:Date, value:Double, name:String):Option[DividendCurve] = 
    apply(valuedate, Map(new qlPeriod("1Y") -> value))
    
  def apply(valuedate:Date, values:Map[qlPeriod, Double]):Option[DividendCurve] = 
    Some(DividendCurve(buildCurve(valuedate, values)))

}