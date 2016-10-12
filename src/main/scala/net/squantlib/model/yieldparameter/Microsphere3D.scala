package net.squantlib.model.yieldparameter

import scala.collection.immutable.SortedMap
import org.jquantlib.time.{Period => qlPeriod}
import org.apache.commons.math3.analysis.interpolation.MicrosphereInterpolator
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
case class Microsphere3D(var valuedate:Date, values:Map[(Double, Double), Double]) extends YieldParameter3D {
	
	def minValue = values.values.min
	def maxValue = values.values.max
	
    def value(x1:Double, x2:Double):Double = math.max(minValue, math.min(maxValue, interpolation(x1, x2)))

    def interpolation(x1:Double, x2:Double) = sphere.value(Array(x1, x2))

    val sphere = {
	    val keysarray = values.keySet.toArray.map{case (a, b) => Array(a, b)}
	    val valarray = values.values.toArray
	    new MicrosphereInterpolator().interpolate(keysarray, valarray)
    }    

    def shifted(shift:(Double, Double, Double) => Double):Microsphere3D = {
      new Microsphere3D(valuedate, values.map{case ((k1, k2), v) => ((k1, k2), shift(k1, k2, v))})
    }
    
}


object Microsphere3D{
  
  def apply(valuedate:Date, values: => Map[(qlPeriod, qlPeriod), Double]):Microsphere3D = 
    new Microsphere3D(valuedate, values.map{case ((d1, d2), v) => ((valuedate.days(d1).toDouble, valuedate.days(d2).toDouble), v)})
    
//  def apply(valuedate:Date, values:Map[(qlPeriod, qlPeriod), Double]):Microsphere3D = apply(valuedate, values)
  
}
  
  
  
  
  
  
  
  
  