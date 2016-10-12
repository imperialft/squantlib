package net.squantlib.model.yieldparameter

import scala.collection.immutable.SortedMap
import org.jquantlib.time.{Period => qlPeriod}
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.SplineInterpolator
import org.apache.commons.math3.analysis.function.{Log, Exp}
import net.squantlib.util.Date

/**
 * TimeVector with Spline interpolation and exponential extrapolation. (always > 0)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
case class SplineEExtrapolation(var valuedate : Date, values:Map[Double, Double], extrapoints:Int) extends YieldParameter with AbstractYieldParameter {
	require(values.size >= 3, "spline requires at least 3 point : found " + values.size)
	
	val sortedValues = SortedMap(values.toSeq:_*)
	val firstvalue = sortedValues.head._2
	
	override val mindays = sortedValues.firstKey
	override val maxdays = sortedValues.lastKey
	
	val extrapolator = new Exp()
	val logger = new Log()
	
	val impliedRate = if(sortedValues.last._2 < Double.MinPositiveValue) Double.NaN 
				else -1.0 * logger.value(sortedValues.last._2 / firstvalue) / sortedValues.lastKey
	

	override def lowextrapolation(v : Double) = sortedValues.head._2
 
	override def highextrapolation(v : Double) = if (impliedRate.isNaN || impliedRate.isInfinity) Double.MinPositiveValue 
    						else firstvalue * extrapolator.value(-1.0 * impliedRate * v.toDouble)
    						
    def interpolation(v : Double) = splinefunction.value(v)

    val splinefunction:PolynomialSplineFunction = {
    	val extraKeys = (1 to extrapoints).toArray.map(i => sortedValues.lastKey + (30.0 * i.toDouble))
    	val extraValues = (1 to extrapoints).toArray.map(i => sortedValues.last._2)
	    
	    val keysarray = sortedValues.keySet.toArray ++ extraKeys
	    val valarray = sortedValues.values.toArray ++ extraValues
	    new SplineInterpolator().interpolate(keysarray, valarray)
    }    

    override def shifted(shift:(Double, Double) => Double):SplineEExtrapolation = {
      new SplineEExtrapolation(valuedate, values.map{case (k, v) => (k, shift(k, v))}, extrapoints)
    }
    
}


object SplineEExtrapolation{
  
  def apply(valuedate:Date, values: => Map[qlPeriod, Double], extrapoints:Int):SplineEExtrapolation = 
    SplineEExtrapolation(valuedate, values.map{case (p, v) => (valuedate.days(p).toDouble, v)}, extrapoints)
    
  def apply(valuedate:Date, values:Map[qlPeriod, Double]):SplineEExtrapolation = apply(valuedate, values)
  
}
  
  
  
  
  
  
  
  
  