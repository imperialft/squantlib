package squantlib.parameter

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation._
import org.apache.commons.math3.analysis.function.Log
import org.apache.commons.math3.analysis.function.Exp

/**
 * Encapsulate time series vector parameter with interpolation, extrapolation and other adjustments functions.
 * 
 */
trait TimeVector {
	/**
	 * Returns base date of this vector. 
	 */
	var valuedate : JDate
	/**
	 * Returns number of days between value date and first defined point.
	 * This point is the low boundary between interpolation & extrapolation.
	 */
    val minDays : Long
	/**
	 * Returns number of days between value date and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
    val maxDays : Long
	/**
	 * Returns date of final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	val maxDate : JDate
	/**
	 * Returns period between valueDate and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	val maxPeriod: JPeriod

	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days : Long) : Double
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def value(date : JDate) : Double = value(date.serialNumber() - valuedate.serialNumber())
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def value(period : JPeriod) : Double = value(period.days(valuedate))
}

/**
 * Basic Framework for Long-Double Interpolation
 * Points are interpolated between max and min range, and extrapolated outside.
 */
trait AbstractTimeVector {
	var valuedate : JDate
	val minDays : Long
	val maxDays : Long

	def lowextrapolation(v : Long) : Double
    def highextrapolation(v : Long) : Double
    def interpolation(v : Long) : Double
  
    def value(v : Long) : Double = {
      v match {
        case vv if vv <= minDays => lowextrapolation(vv)
        case vv if vv >= maxDays => highextrapolation(vv)
        case _ => interpolation(v)
          }
    }
}

/**
 * TimeVector with Spline interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class SplineNoExtrapolation(var valuedate : JDate, values:SortedMap[JPeriod, Double], extrapoints:Int) extends TimeVector with AbstractTimeVector {
  
	require(values.size > 2)
	
    val splinefunction = {
	    var inputpoints :TreeMap[Long, Double] = TreeMap.empty
	    for (d <- values.keySet) { inputpoints ++= Map(d.days(valuedate) -> values(d)) }
	    for (i <- 1 to extrapoints) { inputpoints ++= Map((values.lastKey.days(valuedate) + (30L * i.toLong)) -> values.last._2) }
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Long) => inputpoints(i))
	    new SplineInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)
    }
    
	val minDays = values.firstKey.days(valuedate)
	val maxDays = values.lastKey.days(valuedate)
	val maxDate = new JDate(valuedate.serialNumber() + maxDays)
	val maxPeriod = new JPeriod(maxDays.toInt, TimeUnit.Days)

	def lowextrapolation(v : Long) = values.first._2
    def highextrapolation(v : Long) = values.last._2
    def interpolation(v : Long) = splinefunction.value(v.toDouble)
}


/**
 * TimeVector with Linear interpolation and no extrapolation. (ie. y-value is constant after last date)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs linear curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class LinearNoExtrapolation(var valuedate : JDate, values:SortedMap[JPeriod, Double]) extends TimeVector with AbstractTimeVector {
    val linearfunction = {
	    var inputpoints :TreeMap[Long, Double] = TreeMap.empty
	    for (d <- values.keySet) { inputpoints ++= Map(d.days(valuedate) -> values(d)) }
	    val keysarray = inputpoints.keySet.toArray
	    val valarray = keysarray.map((i:Long) => inputpoints(i))
	    new LinearInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)    
	}
	
	val minDays = values.firstKey.days(valuedate)
	val maxDays = values.lastKey.days(valuedate)
	val maxDate = new JDate(valuedate.serialNumber() + maxDays)
	val maxPeriod = new JPeriod(maxDays.toInt, TimeUnit.Days)

	def lowextrapolation(v : Long) = values.first._2
    def highextrapolation(v : Long) = values.last._2
    def interpolation(v : Long) = linearfunction.value(v.toDouble)
}

/**
 * TimeVector with Spline interpolation and exponential extrapolation. (always > 0)
 * Date is internally described as number of days since value date, but can be accessed by Date or Period.
 * 
 * @constructor constructs polynomial curve from given points. extra flat points are added after final date to manage overshoot.
 * @param input points
 * @param number of extra points (optional)
 */
class SplineEExtrapolation(var valuedate : JDate, values:SortedMap[JPeriod, Double]) extends TimeVector with AbstractTimeVector {
  
	require(values.size > 2)
	
    val splinefunction = {
		var inputpoints :TreeMap[Long, Double] = TreeMap.empty
		for (d <- values.keySet) { inputpoints ++= Map(d.days(valuedate) -> values(d)) }
		val keysarray = inputpoints.keySet.toArray
		val valarray = keysarray.map((i:Long) => inputpoints(i))
		new SplineInterpolator().interpolate(keysarray.map((i:Long)=>i.toDouble), valarray)
    }
    
	val firstvalue = values.first._2
	val impliedrate = -1.0 * new Log().value(values.last._2 / firstvalue) / values.lastKey.days(valuedate).toDouble
	val extrapolator = new Exp()
	
	val minDays = values.firstKey.days(valuedate)
	val maxDays = values.lastKey.days(valuedate)
	val maxDate = new JDate(valuedate.serialNumber() + maxDays)
	val maxPeriod = new JPeriod(maxDays.toInt, TimeUnit.Days)

	def lowextrapolation(v : Long) = values.first._2
    def highextrapolation(v : Long) = firstvalue * extrapolator.value(-1.0 * impliedrate * v.toDouble)
    def interpolation(v : Long) = splinefunction.value(v.toDouble)
}


