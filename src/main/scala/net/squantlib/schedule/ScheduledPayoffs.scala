package net.squantlib.schedule

import scala.collection.LinearSeq
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.schedule.call.{Callabilities, Callability}
import net.squantlib.schedule.payoff.{Payoff, Payoffs, FixedPayoff}
import scala.collection.JavaConversions._
import net.squantlib.util.Date
import org.jquantlib.time.{BusinessDayConvention, Calendar}
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.daycounters._
import scala.runtime.ZippedTraversable3.zippedTraversable3ToTraversable

case class ScheduledPayoffs(
  scheduledPayoffs:LinearSeq[(CalculationPeriod, Payoff, Callability)],
  valuedate:Option[Date] = None
  ) 
  extends LinearSeq[(CalculationPeriod, Payoff, Callability)]{
  
  scheduledPayoffs.foreach{case (s, p, c) => 
    if (p.isAbsolute) s.daycounter = new Absolute
    s.nominal = p.nominal
  }
  
  lazy val (schedule, payoffs, calls) = scheduledPayoffs.unzip3  match {
    case (s, p, c) => (Schedule(s), Payoffs(p), Callabilities(c))
  }
  
  lazy val coupon = Payoffs(payoffs.dropRight(1))
  
  lazy val redemption = payoffs.last
  
  def isPriceable = payoffs.isPriceable && calls.isPriceable

  def trigCheckPayoff:ScheduledPayoffs = {
    val newcalls = calls.map(c => if (c.isTrigger) Callability(false, c.triggers, None, 0.0, c.inputString, None, c.simulatedFrontier)(c.fixingInfo) else c)
    val newpayoffs = scheduledPayoffs.map{case (cp, p, c) => if (cp.isAbsolute) FixedPayoff(1.0)(p.fixingInfo) else FixedPayoff(0.0)(p.fixingInfo)}
    val newScheduledPayoff = ScheduledPayoffs.noFixing(schedule, Payoffs(newpayoffs), Callabilities(newcalls))
    ScheduledPayoffs(newScheduledPayoff, valuedate)
  }
  
  val underlyings:Set[String] = payoffs.underlyings ++ calls.underlyings
  
  lazy val bonusCoeff = schedule.map(_.dayCount)
  
  lazy val bonusAmount = calls.map(_.bonus + 1.0).toList
  
  def amountToRate(amount:List[Double]) = (amount, bonusCoeff).zipped.map(_ / _)
  
  def currentPayoffs(vd:Date):List[Payoff] = filter{case (d, p, c) => d.isCurrentPeriod(vd)}.map(_._2) (collection.breakOut)
  
  def currentCoupons(vd:Date):List[Payoff] = filter{case (d, p, c) => d.isCurrentPeriod(vd) && !d.isAbsolute}.map(_._2) (collection.breakOut)
  
  def isTriggered:Boolean = calls.isTriggered
  
  def triggeredDate:Option[(CalculationPeriod, Double)] = {
    val trigDays = scheduledPayoffs.filter{case (cp, p, c) => c.fixedTrigger == Some(true)}
    if (trigDays.isEmpty) None else Some(trigDays.map{case (cp, p, c) => (cp, c.redemptionAmount)}.minBy{case (cp, c) => cp.eventDate})
  }
  
  lazy val bonusRate = amountToRate(bonusAmount)
  
  val eventDateLegs:List[List[Date]] = {
    val dates:List[List[Date]] = scheduledPayoffs.map{
      case (d, p, t) if p.isFixed && t.isFixed => List.empty
      case (d, p, t) if p.isFixed => List(p.eventDates(d).last)
      case (d, p, t) => p.eventDates(d)
      }(collection.breakOut)
    
    valuedate match {
      case Some(d) => dates.map(ds => ds.filter(_ gt d))
      case None => dates
    }
  }
  
  val eventDates:List[Date] = eventDateLegs.flatten.toSet.toList.sorted
  
  var defaultDaycounter = new Actual365Fixed
  
  def eventDateYears(basedate:Date):List[Double] = eventDates.map(d => Date.daycount(basedate, d, defaultDaycounter))
  
  val dateMapper:List[List[Int]] = eventDateLegs.map(_.map(eventDates.indexOf(_)))
  
  abstract class withDefault[T] { def defaultValue:T }
    
  implicit object mapValue extends withDefault[Map[String, Double]] { def defaultValue = Map.empty[String, Double]}
  
  implicit object listValue extends withDefault[List[Double]] { def defaultValue = List.fill(underlyings.size)(Double.NaN)}
  
  implicit object doubleValue extends withDefault[Double] { def defaultValue = Double.NaN}
  
  def priceMapper[T](fixings:List[T])(implicit defclass:withDefault[T]):List[List[T]] = dateMapper.map(d => {
    if (d.isEmpty) List(defclass.defaultValue) else d.map(fixings) })
    
  def fixingPrices(fixings:List[Double]):List[List[Double]] = priceMapper(fixings)
  
  def fixingPrices(fixings:List[Map[String, Double]])(implicit d:DummyImplicit):List[List[Map[String, Double]]] = priceMapper(fixings)
    
  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = 
    if (calls.isTrigger) {
      if (calls.isPriceable){
        val trig:List[Option[Double]] = calls.calls.map(_.triggers.values.headOption)
        payoffs.price(priceMapper(fixings), trig, bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
      }
      else List.fill(fixings.size)(Double.NaN)
    }
    else if (calls.isTargetRedemption) {
      payoffs.price(priceMapper(fixings), List.fill(calls.size)(None), bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
    }
    else payoffs.price(priceMapper(fixings))

  def price(fixings:List[Map[String, Double]]):List[Double] = {
    if (calls.isTrigger || calls.isTargetRedemption) {
      if (calls.isPriceable) payoffs.price(priceMapper(fixings), calls.triggers, bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
      else List.fill(fixings.size)(Double.NaN)
    }
    else if (calls.isTargetRedemption) {
      payoffs.price(priceMapper(fixings), List.fill(calls.size)(None), bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
    }
    else payoffs.price(priceMapper(fixings))
  }
  
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
    
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, amountToRate(trigAmount), calls.targetRedemptions, schedule.dayCounts, None)
    
  def price(fixings:List[Double], trigger:List[Option[Double]])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, bonusRate, calls.targetRedemptions, schedule.dayCounts, None)
    
  def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, amountToRate(trigAmount), calls.targetRedemptions, schedule.dayCounts, None)
  
  def price:List[Double] = if (calls.isTrigger) List.fill(payoffs.size)(Double.NaN) else payoffs.price

  def withValueDate(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs, Some(vd))
    
  def after(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => cp.paymentDate gt vd})
  
  def before(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => cp.paymentDate le vd})

  def between(vd:Date, lastvd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => (cp.paymentDate le vd) && (cp.paymentDate gt vd)})
  
  def called(vd:Date, redemAmount:Double, paymentCalendar:Calendar, convention:BusinessDayConvention):ScheduledPayoffs = 
    before(vd).addCashflow(vd, redemAmount, paymentCalendar, convention)
  
  def insert(cp:CalculationPeriod, p:Payoff, c:Callability):ScheduledPayoffs = {
    ScheduledPayoffs(scheduledPayoffs :+ (cp, p, c), valuedate).sorted
  }
  
  def addCashflow(paymentDate:Date, amount:Double, calendar:Calendar, paymentConvention:BusinessDayConvention):ScheduledPayoffs = {
    val cp = CalculationPeriod.simpleCashflow(paymentDate, calendar, paymentConvention)
    val p = Payoff.simpleCashFlow(amount)
    val c = Callability.empty
    insert(cp, p, c)
  }
  
  override def apply(i:Int):(CalculationPeriod, Payoff, Callability) = scheduledPayoffs(i)
  
  def newline = sys.props("line.separator")
  
  def scheduleDescription:(List[String], List[List[String]]) = {
    val title = List("valuedate", "paydate", "payoff", "call", "fixing")
    val sched = scheduledPayoffs.map{case (d, p, c) => 
      List(d.eventDate.toString, 
          d.paymentDate.toString,
          p.toString, 
          c.toString, 
          if (underlyings.isEmpty || (p.variables.isEmpty && c.variables.isEmpty)) "fixed"
          else if (!p.isFixed && !c.isFixed) "not fixed" 
          else "fixed:" + (p.getFixings ++ c.getFixings).map{case (k, v) => k + ":" + v}.mkString(" ")
          )}.toList
    (title, sched)
  }
  
  override def toString = scheduleDescription match {
    case (title, sched) => title.mkString("\t") + newline + sched.map(_.mkString("\t")).mkString(newline)
  }
	
  override def isEmpty:Boolean = scheduledPayoffs.isEmpty
	
  override def head:(CalculationPeriod, Payoff, Callability) = scheduledPayoffs.head
	
  override def tail = scheduledPayoffs.tail
	
  override def length = scheduledPayoffs.size
  
  def filtered(filterFunction:((CalculationPeriod, Payoff, Callability)) => Boolean):ScheduledPayoffs = 
    ScheduledPayoffs(scheduledPayoffs.filter(filterFunction), valuedate)
    
  def mapped(mapFunction:((CalculationPeriod, Payoff, Callability)) => (CalculationPeriod, Payoff, Callability)):ScheduledPayoffs = 
    ScheduledPayoffs(scheduledPayoffs.map(mapFunction), valuedate)
	
  def shifted(days:Int):ScheduledPayoffs = ScheduledPayoffs.noFixing(schedule.shifted(days), payoffs, calls)
    
  override def toList:List[(CalculationPeriod, Payoff, Callability)] = scheduledPayoffs.toList
  
  def isFixed:Boolean = payoffs.isFixed && calls.isFixed
  
  def sorted = ScheduledPayoffs(schedule.sortWith(payoffs, calls), valuedate)
  
}


object ScheduledPayoffs {

  def empty:ScheduledPayoffs = ScheduledPayoffs(Schedule.empty, Payoffs.empty, Callabilities.empty)

//    def accArray(a:List[Double], result:List[Double], acc:Double):List[Double] = 
//      if (a.isEmpty) result.reverse
//      else accArray(a.tail, (a.head + acc) :: result, a.head + acc)
//    
//    val accPayments = accArray(payments, List.empty, 0.0)
//    calls.zip(accPayments).exists{case (c, p) => c.isFixed && c.targetRedemption.collect{case tgt => tgt >= p}.getOrElse(false)}
//  }
  
  
  def apply(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val fixingMap = getFixings(payoffs.underlyings ++ calls.underlyings, schedule.eventDates)
    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
//    if (calls.isTargetRedemption) {calls.assignAccumulatedPayments(schedule, payoffs)}
    calls.assignAccumulatedPayments(schedule, payoffs)
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
    
  def sorted(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val fixingMap = getFixings(payoffs.underlyings ++ calls.underlyings, schedule.eventDates)
    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
//    if (calls.isTargetRedemption) {calls.assignAccumulatedPayments(schedule, payoffs)}
    calls.assignAccumulatedPayments(schedule, payoffs)
    ScheduledPayoffs(schedule.sortWith(payoffs, calls), None)
  }
  
  def getFixings(underlyings:Set[String], dates:List[Date]):List[Map[String, Double]] = 
    DB.pastFixings(underlyings, dates).map(_.collect{case (k, Some(v)) => (k, v)})
    
  def getExterpolatedFixings(underlyings:Set[String], dates:List[Date], valuedate:Date):List[Map[String, Double]] = {
    if (dates.isEmpty) {return List.empty}
    val valuedate = DB.latestParamDate.getOrElse(dates.max)
    val (datesbefore, datesafter) = dates.span(_ le valuedate)
    val fixings = getFixings(underlyings, datesbefore)
    val currentFixings:List[Map[String, Double]] = DB.pastFixings(underlyings, List(valuedate)).map(_.collect{case (k, Some(v)) => (k, v)})
    val exterps:List[Map[String, Double]] = datesafter.map{case _ => 
      if (!currentFixings.isEmpty) currentFixings.last.map{case (k, v) => (k, v)}
      else if (!fixings.isEmpty) fixings.last.map{case (k, v) => (k, v)}
      else Map.empty[String, Double]}
    
    fixings ++ exterps
  }
  
  def extrapolate(schedule:Schedule, payoffs:Payoffs, calls:Callabilities, valuedate:Date):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val (datesbefore, datesafter) = schedule.eventDates.span(_ le valuedate)
    val fixingMap:List[Map[String, Double]] = getExterpolatedFixings(payoffs.underlyings ++ calls.underlyings, datesbefore, valuedate) ++ List.fill(datesafter.size)(Map.empty[String, Double])
    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
//    if (calls.isTargetRedemption) {calls.assignAccumulatedPayments(schedule, payoffs)}
    calls.assignAccumulatedPayments(schedule, payoffs)
    ScheduledPayoffs(schedule.sortWith(payoffs, calls), None)
  }
  
  def noFixing(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
  
}