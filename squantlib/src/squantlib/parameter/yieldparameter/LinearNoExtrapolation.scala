package squantlib.parameter.yieldparameter

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
	
    val linearfunction = {
	    var inputpoints :SortedMap[Long, Double] = SortedMap.empty
	    for (d <- inputvalues.keySet) { inputpoints ++= Map(d.days(valuedate) -> inputvalues(d)) }
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Long) => inputpoints(i))
	    new LinearInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)    
	}
	
	val mindays = inputvalues.firstKey.days(valuedate)
	val maxdays = inputvalues.lastKey.days(valuedate)

	def lowextrapolation(v : Long) = inputvalues.first._2
    def highextrapolation(v : Long) = inputvalues.last._2
    def interpolation(v : Long) = linearfunction.value(v.toDouble)
}
