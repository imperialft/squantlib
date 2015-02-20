package net.squantlib.math.timeseries

import scala.language.implicitConversions
import java.util.{Date => JavaDate}
import org.jquantlib.time.{Period => qlPeriod }
import org.jquantlib.time.Calendar
import scala.collection.{SortedSet, SortedMap}
import net.squantlib.util.Date
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.initializer.Calendars
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation.LinearInterpolator

case class TimeSeries(ts:SortedMap[Date, Double]) extends SortedMap[Date, Double] {
  
  implicit def sortedMapToTS(smap:SortedMap[Date, Double]) = TimeSeries(ts)

  def show = ts.foreach(t => standardOutput(t._1.toString, t._2))
  
  def intersectionWith(ts2:TimeSeries):SortedMap[Date, (Double, Double)] = {
  val commonkeys:SortedSet[Date] = ts.keySet & ts2.keySet
  SortedMap(commonkeys.map(k => (k, (ts(k), ts2(k)))).toSeq :_*)
  }
  
  def tmap(f:SortedMap[Date, Double] => SortedMap[Date, Double]):TimeSeries = TimeSeries(f(ts))
  
  def tmapValues(f:Double => Double):TimeSeries = TimeSeries(ts.mapValues(f))
  
  override def +[T >: Double](ts2:(Date, T)):TimeSeries = ts2 match {
    case (d, v:Double) => TimeSeries(ts.updated(d, v))
    case (d, v) => TimeSeries(ts.updated(d, v.asInstanceOf[Double]))
  }
  
  def deepCopy:TimeSeries = {
    val newts:Map[Date, Double] = ts.map{case (k, v) => (k.copy, v*1)}(collection.breakOut)
    TimeSeries(newts)
  }
  
  override def -(key:Date):TimeSeries = TimeSeries(ts.-(key))
  
  override def iterator:Iterator[(Date, Double)] = ts.iterator
  
  override def get(key:Date):Option[Double] = ts.get(key)
  
  override def rangeImpl(from: Option[Date], until: Option[Date]):TimeSeries = TimeSeries(ts.rangeImpl(from, until)) 
  
  override def ordering = ts.ordering
  
  def filter(f:(Date, Double) => Boolean):TimeSeries = new TimeSeries(ts.filter(f))
  
  override def filterKeys(f:Date => Boolean):TimeSeries = new TimeSeries(ts.filterKeys(f))
  
  def mapValues(f:(Double) => Double):TimeSeries = new TimeSeries(ts.mapValues(f))
  
  def add(ts1:TimeSeries):TimeSeries = {
    val dates = ts1.keySet ++ ts.keySet
    val datemap = dates.map(d => (d, (ts.getOrElse(d, 0.0) + ts1.getOrElse(d, 0.0))))
    TimeSeries(datemap)
  }
  
  def append(ts1:TimeSeries):TimeSeries = TimeSeries(ts ++ ts1)
  
  def firstDate:Date = ts.head._1
  
  def lastDate:Date = ts.last._1
  
  def getFilledTimeSeries(startDate:Date = null, endDate:Date = null):TimeSeries = {
    if (ts.isEmpty) TimeSeries.empty
    else {
      val realStartDate = if((startDate != null) && (startDate ge firstDate)) startDate else firstDate
      val realEndDate = if (endDate == null) lastDate else endDate
      val dates = for(d <- realStartDate.serialNumber to realEndDate.serialNumber) yield (Date(d))
      
      val tsmap = 
        if (ts.size == 1) dates.map(d => (d, ts.head._2))(collection.breakOut)
        else {
          val keysarray = ts.keySet.map(_.serialNumber.toDouble).toArray
          val valarray = ts.values.toArray
          val linearfunction = new LinearInterpolator().interpolate(keysarray, valarray)
          dates.map(d => (d, linearfunction.value(d.serialNumber.toDouble)))(collection.breakOut)
        }
      
      TimeSeries(tsmap)
    }
  }
  
  def getBusinessDayFilledTimeSeries(calendar:Calendar = Calendars.empty, startDate:Date = null, endDate:Date = null):TimeSeries = 
    if (ts.isEmpty) TimeSeries.empty
    else TimeSeries(getFilledTimeSeries(startDate, endDate).filterKeys(d => d.isBusinessday(calendar)))
    
}

object TimeSeries {
  
  def apply(ts:Map[Date, Double]):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply(ts:Map[JavaDate, Double])(implicit d:DummyImplicit):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply[A<:Iterable[(Date, Double)]](ts:A):TimeSeries = TimeSeries(SortedMap(ts.toSeq : _*))
  
  def apply[A<:Iterable[(JavaDate, Double)]](ts:A)(implicit d:DummyImplicit):TimeSeries = TimeSeries(SortedMap(ts.map{case (k, v) => (Date(k), v)}.toSeq : _*))
  
  def empty:TimeSeries = TimeSeries(SortedMap.empty[Date, Double])
}