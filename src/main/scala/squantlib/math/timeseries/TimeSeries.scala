package squantlib.math.timeseries

import org.jquantlib.time.{Date => jDate, Period => qlPeriod }
import scala.collection.{SortedSet, SortedMap}
import squantlib.util.Date
import java.util.{Date => JavaDate}
import org.jquantlib.time.{Weekday}
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

case class TimeSeries(ts:SortedMap[Date, Double]) extends SortedMap[Date, Double] {
  
  implicit def sortedMapToTS(smap:SortedMap[Date, Double]) = TimeSeries(ts)
		
  def show = ts.foreach(t => println(t._1.toString + "\t" + t._2))
  
  def intersectionWith(ts2:TimeSeries):SortedMap[Date, (Double, Double)] = {
	val commonkeys:SortedSet[Date] = ts.keySet & ts2.keySet
	SortedMap(commonkeys.map(k => (k, (ts(k), ts2(k)))).toSeq :_*)
  }
  
  def tmap(f:SortedMap[Date, Double] => SortedMap[Date, Double]):TimeSeries = TimeSeries(f(ts))
  
  def tmapValues(f:Double => Double):TimeSeries = TimeSeries(ts.mapValues(f))
  
  override def +[T >: Double](ts2:(Date, T)):SortedMap[Date, T] = ts + ts2
  
  override def -(key:Date):SortedMap[Date, Double] = ts.-(key)
  
  override def iterator:Iterator[(Date, Double)] = ts.iterator
  
  override def get(key:Date):Option[Double] = ts.get(key)
  
  override def rangeImpl(from: Option[Date], until: Option[Date]) = TimeSeries(ts.rangeImpl(from, until)) 
  
  override def ordering = ts.ordering
  
  def add(ts1:TimeSeries):TimeSeries = {
    val dates = ts1.keySet ++ ts.keySet
    val datemap = dates.map(d => (d, (ts.getOrElse(d, 0.0) + ts1.getOrElse(d, 0.0))))
    TimeSeries(datemap)
  }
  
  def firstDate:Date = ts.head._1
  
  def lastDate:Date = ts.last._1
  
  def getFilledTimeSeries(skipWeekends:Boolean = true, startDate:Date = firstDate, endDate:Date = lastDate):TimeSeries = {
    val linearfunction:PolynomialSplineFunction = {
      val keysarray = ts.keySet.map(_.serialNumber.toDouble).toArray
      val valarray = ts.values.toArray
      new LinearInterpolator().interpolate(keysarray, valarray)
    }
    val realStartDate = if(startDate ge firstDate) startDate else firstDate
    val dates = for(d <- realStartDate.serialNumber to endDate.serialNumber) yield (Date(d))
    val valuedates:Map[Date, Double] = (if (skipWeekends) dates.withFilter(d => d.isWeekday) else dates).map(d => (d, linearfunction.value(d.serialNumber.toDouble)))(collection.breakOut)
    TimeSeries(valuedates)
  }
    
}

object TimeSeries {
  
//  def apply(ts:Map[Date, Double]):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply[A<:Iterable[(Date, Double)]](ts:A):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply[A<:Iterable[(JavaDate, Double)]](ts:A)(implicit d:DummyImplicit):TimeSeries = TimeSeries(SortedMap(ts.map{case (k, v) => (Date(k), v)}.toSeq : _*))
  
}

