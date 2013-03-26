package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate}
import org.jquantlib.daycounters.Actual365Fixed

case class ScheduledPayoffs(
    scheduledPayoffs:LinearSeq[(CalculationPeriod, Payoff, Callability)],
    valuedate:Option[qlDate] = None) 
extends LinearSeq[(CalculationPeriod, Payoff, Callability)]{
  
  lazy val (schedule, payoffs, calls) = scheduledPayoffs.unzip3  match {
    case (s, p, c) => (Schedule(s), Payoffs(p), Callabilities(c))
  }
  
  lazy val coupon = Payoffs(payoffs.dropRight(1))
  
  lazy val redemption = payoffs.last

  val variables:Set[String] = payoffs.variables
  
  lazy val bonusCoeff = schedule.map(_.dayCount)
  
  lazy val bonusAmount = calls.map(_.bonus + 1.0).toList
  
  def amountToRate(amount:List[Double]) = (amount, bonusCoeff).zipped.map(_ / _)
  
  lazy val bonusRate = amountToRate(bonusAmount)
  
  val eventDateLegs:List[List[qlDate]] = scheduledPayoffs.map{
    case (d, p, t) if valuedate.isDefined && (d.eventDate le valuedate.get) => List.empty
    case (d, p, t) if p.variables.isEmpty && t.isEmpty => List.empty
    case (d, p, t) if p.variables.isEmpty => List(p.eventDates(d).last)
    case (d, p, t) => p.eventDates(d)
    }.toList
  
  val eventDates:List[qlDate] = eventDateLegs.flatten.toSet.toList.sorted
  
  var defaultDaycounter = new Actual365Fixed
  
  def eventDateYears(basedate:qlDate):List[Double] = eventDates.map(d => defaultDaycounter.yearFraction(basedate, d))
  
  val dateMapper:List[List[Int]] = eventDateLegs.map(_.map(eventDates.indexOf(_)))
  
  abstract class withDefault[T] { def defaultValue:T }
    
  implicit object mapValue extends withDefault[Map[String, Double]] { def defaultValue = Map.empty[String, Double]}
  
  implicit object doubleValue extends withDefault[Double] { def defaultValue = Double.NaN}
  
  def priceMapper[T](fixings:List[T])(implicit defclass:withDefault[T]):List[List[T]] = dateMapper.map(d => {
    if (d.isEmpty) List(defclass.defaultValue) else d.map(fixings) })
    
  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = 
    if (calls.isTrigger) {
      val trig = calls.toList.map(_.triggers.headOption.getOrElse(None))
      payoffs.price(priceMapper(fixings), trig, bonusRate)
    } 
    else payoffs.price(priceMapper(fixings))

  def price(fixings:List[Map[String, Double]]):List[Double] = 
    if (calls.isTrigger) payoffs.price(priceMapper(fixings), calls.triggerMap, bonusRate)
    else payoffs.price(priceMapper(fixings))
    
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, bonusRate)
    
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, amountToRate(trigAmount))
    
  def price(fixings:List[Double], trigger:List[Option[Double]])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, bonusRate)
    
  def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, amountToRate(trigAmount))
  
  def price:List[Double] = payoffs.price

  def withValueDate(vd:qlDate):ScheduledPayoffs = ScheduledPayoffs(filter{case (cp, _, _) => cp.paymentDate gt vd}, Some(vd))
  
  
  override def apply(i:Int):(CalculationPeriod, Payoff, Callability) = scheduledPayoffs(i)
  
  override def toString = scheduledPayoffs.map{case (d, p, c) => d.toString + " " + p.toString + " " + c.toString}.mkString("\n")
	
  override def isEmpty:Boolean = scheduledPayoffs.isEmpty
	
  override def head:(CalculationPeriod, Payoff, Callability) = scheduledPayoffs.head
	
  override def tail = scheduledPayoffs.tail
	
  override def length = scheduledPayoffs.size
  
  def filtered(filterFunction:((CalculationPeriod, Payoff, Callability)) => Boolean):ScheduledPayoffs = 
    ScheduledPayoffs(scheduledPayoffs.filter(filterFunction), valuedate)
    
  def mapped(mapFunction:((CalculationPeriod, Payoff, Callability)) => (CalculationPeriod, Payoff, Callability)):ScheduledPayoffs = 
    ScheduledPayoffs(scheduledPayoffs.map(mapFunction), valuedate)
	
//  override def iterator:Iterator[(CalculationPeriod, Payoff, Callability)] = scheduledPayoffs.
	
  override def toList:List[(CalculationPeriod, Payoff, Callability)] = scheduledPayoffs.toList
  
  }


object ScheduledPayoffs {

  def empty:ScheduledPayoffs = ScheduledPayoffs(Schedule.empty, Payoffs.empty, Callabilities.empty)
  
  def apply(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = 
    ScheduledPayoffs((schedule, payoffs, calls).zip)
    
  def sorted(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = 
    ScheduledPayoffs(schedule.sortWith(payoffs, calls))
  
//  def apply(payoffschedule:LinearSeq[(CalculationPeriod, Payoff, Callability)]):ScheduledPayoffs = 
//    payoffschedule.unzip3 match { case (s, po, c) => ScheduledPayoffs(Schedule(s), Payoffs(po), Callabilities(c))}
  
}