package net.squantlib.model.yieldparameter

import scala.collection.mutable.MutableList
import org.jquantlib.time.{Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.DayCounter
import net.squantlib.util.Date

/**
 * Encapsulate time series vector parameter with interpolation, extrapolation and other adjustments functions.
 */
trait YieldParameter3D {
  
	/**
	 * Returns base date of this vector. 
	 */
	var valuedate : Date
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(d1:Double, d2:Double) : Double
    def apply(d1:Double, d2:Double) = value(d1, d2)
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(d1:Long, d2:Long) : Double = value(d1.toDouble, d2.toDouble)
    def apply(d1:Long, d2:Long) = value(d1, d2)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as day count fraction and its day count method.
	 */
    def value(dayfrac1:Double, dayfrac2:Double, dayCounter:DayCounter) : Double = value(dayfrac1 * 365.0 / dayCounter.annualDayCount, dayfrac2 * 365.0 / dayCounter.annualDayCount)
    def apply(dayfrac1:Double, dayfrac2:Double, dayCounter:DayCounter) = value(dayfrac1, dayfrac2, dayCounter)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def value(d1:Date, d2:Date) : Double = value(d1.serialNumber - valuedate.serialNumber, d2.serialNumber - valuedate.serialNumber)
    def apply(d1:Date, d2:Date) = value(d1, d2)
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def value(period1:qlPeriod, period2:qlPeriod):Double = value(valuedate.days(period1), valuedate.days(period2) + valuedate.days(period1))
    def apply(period1:qlPeriod, period2:qlPeriod) = value(period1, period2)
    
	/**
	 * Returns yield parameter with curve shifted by given formula
	 * @param function mapping (Days, CurrentRate) to NewRate
	 */
    def shifted(f:(Double, Double, Double) => Double):YieldParameter3D
    
	
//  /**
//   * Returns a String representation of this object.
//   */
//    override def toString:String = {
//      getClass + " (" + valuedate.add(mindays.toInt) + " to " + valuedate.add(maxdays.toInt) + ")"
//    }
    
    def show:Unit = ((1 to 10) ++ List(12, 15, 20, 25, 30)).map(i => {
      val m1 = new qlPeriod(i, TimeUnit.Years)
      printf("\n" + m1.toString)
      List(1, 5, 10, 20).map(j => {
        val m2 = new qlPeriod(j, TimeUnit.Years)
        printf("\t" + m2.toString + ":" + value(m1, m2).toString)})})
}

object YieldParameter3D {
  
	def apply(valuedate:Date, data:Map[(Double, Double), Double]):YieldParameter3D 
	= Microsphere3D(valuedate, data)
	
	def apply(valuedate:Date, data: => Map[(qlPeriod, qlPeriod), Double]):YieldParameter3D 
	= Microsphere3D(valuedate, data)
}
