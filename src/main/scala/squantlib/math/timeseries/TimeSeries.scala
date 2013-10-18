package squantlib.math.timeseries

import org.jquantlib.time.{Date => qlDate, Period => qlPeriod }
import scala.collection.{SortedSet, SortedMap}
import java.util.{Date => JavaDate}
import org.jquantlib.time.{Weekday, Date => qlDate}
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

case class TimeSeries(ts:SortedMap[qlDate, Double]) extends SortedMap[qlDate, Double] {
  
  implicit def sortedMapToTS(smap:SortedMap[qlDate, Double]) = TimeSeries(ts)
		
  def show = ts.foreach(t => println(t._1.shortDate.toString + "\t" + t._2))
  
  def intersectionWith(ts2:TimeSeries):SortedMap[qlDate, (Double, Double)] = {
	val commonkeys:SortedSet[qlDate] = ts.keySet & ts2.keySet
	SortedMap(commonkeys.map(k => (k, (ts(k), ts2(k)))).toSeq :_*)
  }
  
  override def +[T >: Double](ts2:(qlDate, T)):SortedMap[qlDate, T] = ts + ts2
  
  override def -(key:qlDate):SortedMap[qlDate, Double] = ts.-(key)
  
  override def iterator:Iterator[(qlDate, Double)] = ts.iterator
  
  override def get(key:qlDate):Option[Double] = ts.get(key)
  
  override def rangeImpl(from: Option[qlDate], until: Option[qlDate]) = TimeSeries(ts.rangeImpl(from, until)) 
  
  override def ordering = ts.ordering
  
  def firstDate:qlDate = ts.head._1
  
  def lastDate:qlDate = ts.last._1
  
  def getFilledTimeSeries(skipWeekends:Boolean = true, startDate:qlDate = firstDate, endDate:qlDate = lastDate):TimeSeries = {
    val linearfunction:PolynomialSplineFunction = {
      val keysarray = ts.keySet.map(_.serialNumber.toDouble).toArray
      val valarray = ts.values.toArray
      new LinearInterpolator().interpolate(keysarray, valarray)
    }
    val realStartDate = if(startDate ge firstDate) startDate else firstDate
    val dates = for(d <- realStartDate.serialNumber to endDate.serialNumber) yield (new qlDate(d))
    val valuedates:Map[qlDate, Double] = (if (skipWeekends) dates.withFilter(d => isWeekday(d)) else dates).map(d => (d, linearfunction.value(d.serialNumber.toDouble)))(collection.breakOut)
    TimeSeries(valuedates)
  }
  
  def isWeekday(d:qlDate):Boolean = d.weekday != Weekday.Saturday && d.weekday != Weekday.Sunday
    
}

object TimeSeries {
  
  def apply(ts:Map[qlDate, Double]):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply(ts:Map[JavaDate, Double])(implicit d:DummyImplicit):TimeSeries = TimeSeries(SortedMap(ts.map{case (k, v) => (new qlDate(k), v)}.toSeq : _*))
  
}