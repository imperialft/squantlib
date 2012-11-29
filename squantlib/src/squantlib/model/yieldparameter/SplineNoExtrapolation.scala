package squantlib.model.yieldparameter

//import scala.collection.immutable.TreeMap
import scala.collection.SortedMap
import scala.collection.Map
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.SplineInterpolator
import org.apache.commons.math3.analysis.function.Log
import org.apache.commons.math3.analysis.function.Exp


/**
 * TimeVector with Spline interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class SplineNoExtrapolation(var valuedate : JDate, values:Map[JPeriod, Double], extrapoints:Int) extends YieldParameter with AbstractYieldParameter {
	require(values.size >= 3, "spline requires at least 3 point : found " + values.size)
	
	val inputvalues = SortedMap(values.toSeq:_*)
	
    val splinefunction = {
	    var inputpoints:SortedMap[Double, Double] = SortedMap.empty
	    
	    for (d <- inputvalues.keySet) 
	      { inputpoints ++= Map(d.days(valuedate).toDouble -> inputvalues(d)) }

	    for (i <- 1 to extrapoints) 
	      { inputpoints ++= Map((inputvalues.lastKey.days(valuedate).toDouble + (30.0 * i.toDouble)).toDouble -> inputvalues.last._2) }
	    
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Double) => inputpoints(i))
	    new SplineInterpolator().interpolate(keysarray, valarray)
    }
    
	val mindays = inputvalues.firstKey.days(valuedate).toDouble
	val maxdays = inputvalues.lastKey.days(valuedate).toDouble


	def lowextrapolation(v : Double) = inputvalues.first._2
    def highextrapolation(v : Double) = inputvalues.last._2
    def interpolation(v : Double) = splinefunction.value(v)
}
