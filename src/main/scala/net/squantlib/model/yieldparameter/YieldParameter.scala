package net.squantlib.model.yieldparameter

import scala.collection.mutable.MutableList
import org.jquantlib.time.{Period => qlPeriod, TimeUnit }
import org.jquantlib.daycounters.DayCounter
import net.squantlib.util.Date

/**
 * Encapsulate time series vector parameter with interpolation, extrapolation and other adjustments functions.
 */
trait YieldParameter extends Iterable[Pair[Date, Double]] {
  
	/**
	 * Returns base date of this vector. 
	 */
	var valuedate : Date
	/**
	 * Returns number of days between value date and first defined point.
	 * This point is the low boundary between interpolation & extrapolation.
	 */
    val mindays : Double
    
	/**
	 * Returns number of days between value date and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
    val maxdays : Double
    
	/**
	 * Returns date of final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	def maxdate : Date = Date(valuedate.serialNumber + maxdays.toLong)
	
	/**
	 * Returns period between valueDate and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
	def maxperiod = new qlPeriod(maxdays.toInt, TimeUnit.Days)
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days:Double) : Double
    def apply(days:Double) = value(days)
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days:Long) : Double = value(days.toDouble)
    def apply(days:Long) = value(days)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as day count fraction and its day count method.
	 */
    def value(dayfrac : Double, dayCounter:DayCounter) : Double = value((dayfrac * 365.0 / dayCounter.annualDayCount))
    def apply(dayfrac : Double, dayCounter:DayCounter) = value(dayfrac, dayCounter)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def value(date : Date) : Double = value(date.serialNumber - valuedate.serialNumber)
    def apply(date : Date) = value(date)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def value(period : qlPeriod) : Double = value(valuedate.days(period))
    def apply(date:qlPeriod) = value(date)
    
	/**
	 * Returns yield parameter with curve shifted by given formula
	 * @param function mapping (Days, CurrentRate) to NewRate
	 */
    def shifted(f:(Double, Double) => Double):YieldParameter
    
  /**
   * Returns an Iterator that provides data during mindays..maxdays incremented by 1 day
   */
    def iterator:Iterator[Pair[Date, Double]] = {
      // FIXME: This could be inefficient.
      val list = MutableList[Pair[Date, Double]]()
      for (i <- mindays.toLong to maxdays.toLong)
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

object YieldParameter {
  
  def apply(valuedate:Date, data:Map[Double, Double]):Option[YieldParameter] = data.size match {
    case s if s > 2 => Some(SplineNoExtrapolation(valuedate, data))
    case 2 => Some(LinearNoExtrapolation(valuedate, data))
    case 1 => Some(FlatVector(valuedate, data.head._2))
    case _ => None
  }
  
  def apply(valuedate:Date, data:Map[Date, Double])(implicit d:DummyImplicit):Option[YieldParameter] = {
    val values = data.withFilter{case (d, v) => (d ge valuedate)}
    			.map{case (k, v) => (k.serialNumber.toDouble - valuedate.serialNumber.toDouble, v)}.toMap
    apply(valuedate, values)
  }
  
  def apply(valuedate:Date, data:Map[qlPeriod, Double])(implicit d:DummyImplicit, e:DummyImplicit):Option[YieldParameter] = {
    val values = data.map{case (p, v) => (valuedate.days(p).toDouble, v)}.toMap
    apply(valuedate, values)
  }
	
  def apply(valuedate:Date, value:Double):Option[YieldParameter] = Some(FlatVector(valuedate, value))
}
