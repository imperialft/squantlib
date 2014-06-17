package net.squantlib.model.yieldparameter

import scala.collection.SortedMap
import scala.collection.Map
import net.squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod}
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

/**
 * TimeVector with Linear interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs linear curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 */
case class LinearNoExtrapolation(var valuedate : Date, values:Map[Double, Double]) extends YieldParameter with AbstractYieldParameter {
	require(values.size >= 2, "linear interpolation requires at least 2 point : found " + values.size)
  
	val sortedValues = SortedMap(values.toSeq:_*)
	
  val linearfunction:PolynomialSplineFunction = {
    val keysarray = sortedValues.keySet.toArray
    val valarray = sortedValues.values.toArray
    new LinearInterpolator().interpolate(keysarray, valarray)    
	}
	
	override val mindays = sortedValues.firstKey
	override val maxdays = sortedValues.lastKey

  override def lowextrapolation(v : Double) = sortedValues.head._2
    override def highextrapolation(v : Double) = sortedValues.last._2
    override def interpolation(v : Double) = linearfunction.value(v)
    
    override def shifted(shift:(Double, Double) => Double):LinearNoExtrapolation = 
      new LinearNoExtrapolation(valuedate, values.map{case (k, v) => (k, shift(k, v))})
}


object LinearNoExtrapolation {
  
  def apply(valuedate : Date, values: => Map[qlPeriod, Double]):YieldParameter = 
    values.map{case (p, v) => (valuedate.days(p).toDouble, v)} match {
      case vs if vs.size == 1 => FlatVector(valuedate, vs.head._2)
      case vs => LinearNoExtrapolation(valuedate, vs)
    }
  
}