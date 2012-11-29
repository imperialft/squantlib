package squantlib.model.yieldparameter

import scala.collection.SortedMap
import scala.collection.Map
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

/**
 * TimeVector with Linear interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs linear curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 */
class LinearNoExtrapolation(var valuedate : JDate, values:Map[JPeriod, Double]) extends YieldParameter with AbstractYieldParameter {
	require(values.size >= 2, "linear interpolation requires at least 2 point : found " + values.size)
  
	val inputvalues = SortedMap(values.toSeq:_*)
	
    val linearfunction:PolynomialSplineFunction = {
	    var inputpoints:SortedMap[Double, Double] = SortedMap.empty
	    
	    for (d <- inputvalues.keySet) 
	    { inputpoints ++= Map(d.days(valuedate).toDouble -> inputvalues(d)) }
	    
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Double) => inputpoints(i))
	    new LinearInterpolator().interpolate(keysarray, valarray)    
	}
	
	val mindays = inputvalues.firstKey.days(valuedate).toDouble
	val maxdays = inputvalues.lastKey.days(valuedate).toDouble

	def lowextrapolation(v : Double) = inputvalues.first._2
    def highextrapolation(v : Double) = inputvalues.last._2
    def interpolation(v : Double) = linearfunction.value(v)
}
 