package squantlib.model.yieldparameter

import scala.collection.immutable.SortedMap
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod, TimeUnit }
import org.apache.commons.math3.analysis.interpolation.MicrosphereInterpolator
import org.apache.commons.math3.analysis.function.{Log, Exp}

/**
 * TimeVector with Spline interpolation and exponential extrapolation. (always > 0)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
case class Microsphere3D(var valuedate:qlDate, values:Map[(Double, Double), Double]) extends YieldParameter3D {
	require(values.size >= 3, "spline requires at least 3 point : found " + values.size)
	
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
      new Microsphere3D(valuedate, values.map{case ((k1, k2), v) => ((k1, k2), shift(k1, k2, v))}.toMap)
    }
    
}


object Microsphere3D{
  
  def apply(valuedate:qlDate, values: => Map[(qlPeriod, qlPeriod), Double]):Microsphere3D = 
    new Microsphere3D(valuedate, values.map{case ((d1, d2), v) => ((d1.days(valuedate).toDouble, d2.days(valuedate).toDouble), v)}.toMap)
    
//  def apply(valuedate:qlDate, values:Map[(qlPeriod, qlPeriod), Double]):Microsphere3D = apply(valuedate, values)
  
}
  
  
  
  
  
  
  
  
  