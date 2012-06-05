package squantlib.parameter

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation._

/**
 * Encapsulate time series vector parameter with interpolation, extrapolation and other adjustments functions.
 * 
 */
trait TimeVector {
    var valuedate : JDate

    def value(days : Long) : Double
    def value(date : JDate) : Double = value(date.serialNumber() - valuedate.serialNumber())
    def value(period : JPeriod) : Double = value(period.days(valuedate))
}

/**
 * Basic Framework for Long-Double Interpolation
 * 
 */
trait TimeVectorFrame {
	val Xmin : Long
	val Xmax : Long

	def lowextrapolation(v : Long) : Double
    def highextrapolation(v : Long) : Double
    def interpolation(v : Long) : Double
  
    def value(v : Long) : Double = {
      v match {
        case vv if vv <= Xmin => lowextrapolation(vv)
        case vv if vv >= Xmax => highextrapolation(vv)
        case _ => interpolation(v)
          }
    }
}

/**
 * Returns interpolated value for a given date.
 * Uses Spline interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is described as number of days since value date by construction, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class SplineNoExtrap(var valuedate : JDate, values:SortedMap[JPeriod, Double], extrapoints:Int) extends TimeVector with TimeVectorFrame {
  
	require(values.size > 2)
	
    val splinefunction = {
	    var inputpoints :TreeMap[Long, Double] = TreeMap.empty
	    for (d <- values.keySet) { inputpoints ++= Map(d.days(valuedate) -> values(d)) }
	    for (i <- 1 to extrapoints) { inputpoints ++= Map((values.lastKey.days(valuedate) + (30L * i.toLong)) -> values.last._2) }
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Long) => inputpoints(i))
	    new SplineInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)
    }
    
	val Xmin = values.firstKey.days(valuedate)
	val Xmax = values.lastKey.days(valuedate)

	def lowextrapolation(v : Long) = values.first._2
    def highextrapolation(v : Long) = values.last._2
    def interpolation(v : Long) = splinefunction.value(v.toDouble)
}


/**
 * Returns interpolated value for a given date.
 * Uses Linear interpolation and no extrapolation. (ie. y-value is constant after last date)
 * 
 * @constructor constructs linear curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class LinearNoExtrap(var valuedate : JDate, values:SortedMap[JPeriod, Double]) extends TimeVector with TimeVectorFrame {
    val linearfunction = {
	    var inputpoints :TreeMap[Long, Double] = TreeMap.empty
	    for (d <- values.keySet) { inputpoints ++= Map(d.days(valuedate) -> values(d)) }
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Long) => inputpoints(i))
	    new LinearInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)    
	}
	
	val Xmin = values.firstKey.days(valuedate)
	val Xmax = values.lastKey.days(valuedate)

	def lowextrapolation(v : Long) = values.first._2
    def highextrapolation(v : Long) = values.last._2
    def interpolation(v : Long) = linearfunction.value(v.toDouble)
}

