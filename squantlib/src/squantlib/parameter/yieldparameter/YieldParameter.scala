package squantlib.parameter.yieldparameter

import scala.collection.mutable.MutableList
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod, TimeUnit }
import org.jquantlib.daycounters.DayCounter

/**
 * Encapsulate time series vector parameter with interpolation, extrapolation and other adjustments functions.
 */
trait YieldParameter extends Iterable[Pair[qlDate, Double]] {
	/**
	 * Returns base date of this vector. 
	 */
	var valuedate : qlDate
	/**
	 * Returns number of days between value date and first defined point.
	 * This point is the low boundary between interpolation & extrapolation.
	 */
    val mindays : Long
	/**
	 * Returns number of days between value date and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
    val maxdays : Long
	/**
	 * Returns date of final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	def maxdate : qlDate = new qlDate(valuedate.serialNumber() + maxdays)
	/**
	 * Returns period between valueDate and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	def maxperiod = new qlPeriod(maxdays.toInt, TimeUnit.Days)
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days : Long) : Double
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as day count fraction and its day count method.
	 */
    def value(dayfrac : Double, dayCounter:DayCounter) : Double = value((dayfrac * 365 / dayCounter.annualDayCount).toLong)
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def value(date : qlDate) : Double = value(date.serialNumber() - valuedate.serialNumber())
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def value(period : qlPeriod) : Double = value(period.days(valuedate))
  /**
   * Returns an Iterator that provides data during mindays..maxdays incremented by 1 day
   */
    def iterator:Iterator[Pair[qlDate, Double]] = {
      // FIXME: This could be inefficient.
      val list = MutableList[Pair[qlDate, Double]]()
      for (i <- mindays to maxdays)
        list += Pair(valuedate.add(i.toInt), value(i)) // .toInt, srsly?
      return list.iterator
    }
  /**
   * Returns a String representation of this object.
   */
    override def toString:String = {
      getClass + " (" + valuedate.add(mindays.toInt) + " to " + valuedate.add(maxdays.toInt) + ")"
    }
    
    def describe:String = ((1 to 10) ++ List(12, 15, 20, 25, 30)).map(i => {val m = new qlPeriod(i, TimeUnit.Years); m.toString + " " + value(m).toString + sys.props("line.separator")}).mkString("")
}
