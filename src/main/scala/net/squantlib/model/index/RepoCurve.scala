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
case class RepoCurve(rate:YieldParameter) extends YieldParameter {
  
  def value(d:Double) = rate(d)
  
  var valuedate = rate.valuedate
  
  val mindays = rate.mindays
  
  val maxdays = rate.maxdays
  
  override def shifted(v:(Double, Double) => Double):RepoCurve = RepoCurve(rate.shifted(v))
}


object RepoCurve{
  
	def buildCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => FlatVector(valuedate, values)
			case 2 => LinearNoExtrapolation(valuedate, values)
			case _ => SplineNoExtrapolation(valuedate, values, 2) } 
	
	def apply(valuedate:Date, value:Double):Option[RepoCurve] 
		= apply(valuedate, Map(new qlPeriod("1Y") -> value))
		
	def apply(valuedate:Date, values:Map[qlPeriod, Double]):Option[RepoCurve] 
		= Some(RepoCurve(buildCurve(valuedate, values)))
		
	def zeroCurve(valuedate:Date):RepoCurve = RepoCurve(FlatVector(valuedate, 0.0))

}