package squantlib.model.index

import squantlib.model.yieldparameter.YieldParameter
import squantlib.setting.initializer.Currencies
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}
import org.jquantlib.currencies.Currency
import squantlib.model.yieldparameter._


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
  
  def buildCurve(valuedate:qlDate, values:Map[qlPeriod, Double]):YieldParameter = 
    (values.keySet.size) match {
    case 1 => FlatVector(valuedate, values)
	case 2 => LinearNoExtrapolation(valuedate, values)
	case _ => SplineNoExtrapolation(valuedate, values, 2) } 
	
  def apply(valuedate:qlDate, value:Double, name:String):Option[DividendCurve] = 
    apply(valuedate, Map(new qlPeriod("1Y") -> value))
    
  def apply(valuedate:qlDate, values:Map[qlPeriod, Double]):Option[DividendCurve] = 
    Some(DividendCurve(buildCurve(valuedate, values)))

}