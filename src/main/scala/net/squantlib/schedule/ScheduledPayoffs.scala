package net.squantlib.schedule

import scala.collection.LinearSeq
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.schedule.call.{Callabilities, Callability}
import net.squantlib.schedule.payoff.{FixedPayoff, Payoff, Payoffs}

import scala.collection.JavaConversions._
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixingInfo}
import org.jquantlib.time.{BusinessDayConvention, Calendar}
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.daycounters._

import scala.runtime.ZippedTraversable3.zippedTraversable3ToTraversable

case class ScheduledPayoffs(
  scheduledPayoffs:LinearSeq[(CalculationPeriod, Payoff, Callability)],
  valuedate:Option[Date] = None
) extends LinearSeq[(CalculationPeriod, Payoff, Callability)]{
  
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
    val newcalls = calls.map(c => {
      if (c.isTrigger) 
        Callability(
          bermudan = false,
          triggers = c.triggers,
          triggerUp = c.triggerUp,
          targetRedemption = None,
          forward = Map.empty, //c.forward,
          bonusAmount = 0.0, //c.bonusAmount,
          removeSatisfiedTriggers = c.removeSatisfiedTriggers,
          inputString = c.inputString,
          accumulatedPayments = None,
          simulatedFrontier= c.simulatedFrontier)(c.fixingInfo) 

        else c
    })

    val newpayoffs = scheduledPayoffs.map{case (cp, p, c) => 
      if (cp.isRedemption) FixedPayoff(1.0)(p.fixingInfo) 
      else FixedPayoff(0.0)(p.fixingInfo)
    }

    val newScheduledPayoff = ScheduledPayoffs.noFixing(schedule, Payoffs(newpayoffs), Callabilities(newcalls))
    ScheduledPayoffs(newScheduledPayoff, valuedate)
  }
  
  val underlyings:Set[String] = payoffs.underlyings ++ calls.underlyings

  val singleUnderlying:Option[String] = underlyings.headOption match {
    case Some(ul) if underlyings.size == 1 => Some(ul)
    case _ => None
  }
  
  lazy val bonusCoeff = schedule.map(_.dayCount)
  
  lazy val terminationAmounts:List[Double] = calls.map(_.bonusAmount + 1.0).toList

  lazy val bonusAmounts:List[Double] = calls.map(_.bonusAmount).toList

  lazy val triggerUps:List[Boolean] = calls.map(_.triggerUp).toList

  lazy val forwardStrikes:List[Option[Map[String, Double]]] = calls.map(c => if (c.forward.isEmpty) None else Some(c.forward)).toList
  
  def amountToRate(amount:List[Double]) = (amount, bonusCoeff).zipped.map(_ / _)
  
  def currentPayoffs(vd:Date):List[Payoff] = filter{case (d, p, c) => d.isCurrentPeriod(vd)}.map(_._2) (collection.breakOut)
  
  def currentCoupons(vd:Date):List[Payoff] = filter{case (d, p, c) => d.isCurrentPeriod(vd) && !d.isRedemption}.map(_._2) (collection.breakOut)
  
  def isTriggered:Boolean = calls.isTriggered
  
  def triggeredDate:Option[(CalculationPeriod, Double)] = {
    val trigDays = scheduledPayoffs.filter{case (cp, p, c) => c.isFixed && c.fixedTrigger == Some(true)}
    
    if (trigDays.isEmpty) None 
    else {
      val dateRedemptionAmounts = trigDays.map{case (cp, p, c) => (cp, c.fixedRedemptionAmount)}.collect{case (cp, Some(cc)) => (cp, cc)}
      if (dateRedemptionAmounts.isEmpty) None else Some(dateRedemptionAmounts.minBy{case (cp, c) => cp.eventDate})
    }
  }
  
//  lazy val terminationRates = amountToRate(terminationAmounts)


  var defaultDaycounter = new Actual365Fixed

  def computeEventDateLegs:List[List[Date]] = {
    val dates: List[List[Date]] = scheduledPayoffs.map {
      case (d, p, t) if p.isFutureFixing => p.eventDates(d)
      case (d, p, t) if p.isPaymentFixed && t.isFixed => List.empty
      case (d, p, t) if p.isPaymentFixed => List(p.eventDates(d).last)
      case (d, p, t) => p.eventDates(d)
    }(collection.breakOut)

    valuedate match {
      case Some(d) => dates.map(ds => ds.filter(_ gt d))
      case None => dates
    }
  }

  def computeEventDates:List[Date] = eventDateLegs.flatten.toSet.toList.sorted

  def computeDateMapper:List[List[Int]] = eventDateLegs.map(_.map(eventDates.indexOf(_)))

  var eventDateLegs:List[List[Date]] = computeEventDateLegs

  var eventDates:List[Date] = computeEventDates

  var dateMapper:List[List[Int]] = computeDateMapper

  def eventDateYears(basedate:Date):List[Double] = eventDates.map(d => Date.daycount(basedate, d, defaultDaycounter))

  def clearFixings:Unit = {
    payoffs.foreach(_.clearFixings)
    calls.foreach(_.clearFixings)
    eventDateLegs = computeEventDateLegs
    eventDates = computeEventDates
    dateMapper = computeDateMapper
  }

  abstract class withDefault[T] { def defaultValue:T }
    
  implicit object mapValue extends withDefault[Map[String, Double]] { def defaultValue = Map.empty[String, Double]}
  
  implicit object listValue extends withDefault[List[Double]] { def defaultValue = List.fill(underlyings.size)(Double.NaN)}
  
  implicit object doubleValue extends withDefault[Double] { def defaultValue = Double.NaN}
  
  def priceMapper[T](fixings:List[T])(implicit defclass:withDefault[T]):List[List[T]] = dateMapper.map(d => {
    if (d.isEmpty) List(defclass.defaultValue) else d.map(fixings) })
    
  def fixingPrices(fixings:List[Double]):List[List[Double]] = priceMapper(fixings)
  
  def fixingPrices(fixings:List[Map[String, Double]])(implicit d:DummyImplicit):List[List[Map[String, Double]]] = priceMapper(fixings)

  def price:List[Double] = if (calls.isTrigger) List.fill(payoffs.size)(Double.NaN) else payoffs.price

  // Single Underlying
  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = singleUnderlying match {
    case Some(ul) => price(fixings.map(f => Map(ul -> f)))
    case _ => List.fill(payoffs.size)(Double.NaN)
  }

  // multiple underlyings
  def price(fixings:List[Map[String, Double]]):List[Double] = payoffs.price(priceMapper(fixings), calls.toList, schedule.dayCounts, None)

  def withValueDate(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs, Some(vd))
    
  //def after(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => cp.paymentDate gt vd})
  private def filterAfter(vd:Date):CalculationPeriod => Boolean = cp => cp.paymentDate gt vd
  
  def after(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, _, _) => filterAfter(vd)(cp)})

  def countAfter(vd:Date):Int = scheduledPayoffs.count{case (cp, _, _) => filterAfter(vd)(cp)}
  
//  def before(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => cp.paymentDate le vd})
  private def filterBefore(vd:Date):CalculationPeriod => Boolean = cp => cp.paymentDate le vd
  
  def before(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, _, _) => filterBefore(vd)(cp)})

  def countBefore(vd:Date):Int = scheduledPayoffs.count{case (cp, _, _) => filterBefore(vd)(cp)}
  
//  def between(vd:Date, lastvd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, p, c) => (cp.paymentDate le vd) && (cp.paymentDate gt vd)})
  private def filterBetween(vdFrom:Date, vdTo:Date):CalculationPeriod => Boolean = cp => (cp.paymentDate le vdTo) && (cp.paymentDate gt vdFrom)
  
  def between(vdFrom:Date, vdTo:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, _, _) => filterBetween(vdFrom, vdTo)(cp)})

  def countBetween(vdFrom:Date, vdTo:Date):Int = scheduledPayoffs.count{case (cp, _, _) => filterBetween(vdFrom, vdTo)(cp)}

  def callShifted(newCalls:LinearSeq[Callability]):ScheduledPayoffs = {
    assert(scheduledPayoffs.size == newCalls.size, s"Number of legs(${scheduledPayoffs.size}) and new callabilities(${newCalls.size})must match")
    ScheduledPayoffs((scheduledPayoffs, newCalls).zipped.map{case ((s, p, c), nc) => (s, p, nc)}, valuedate)
  }

  def called(vd:Date, currencyId:String, paymentCurrencyId:String, redemAmount:Double, paymentCalendar:Calendar, convention:BusinessDayConvention):ScheduledPayoffs =
    before(vd).addCashflow(vd, currencyId, paymentCurrencyId, redemAmount, paymentCalendar, convention, true)
  
  def insert(cp:CalculationPeriod, p:Payoff, c:Callability):ScheduledPayoffs = {
    ScheduledPayoffs(scheduledPayoffs :+ (cp, p, c), valuedate).sorted
  }
  
  def addCashflow(paymentDate:Date, currencyId:String, paymentCurrencyId:String, amount:Double, calendar:Calendar, paymentConvention:BusinessDayConvention, isRedemption: Boolean):ScheduledPayoffs = {
    val cp = CalculationPeriod.simpleCashflow(paymentDate, calendar, paymentConvention, isRedemption)
    val p = Payoff.simpleCashFlow(currencyId, paymentCurrencyId, amount)
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
        else if (!p.isPaymentFixed && !c.isFixed) "not fixed"
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
  
  def isFixed:Boolean = payoffs.isPaymentFixed && calls.isFixed
  
  def sorted = ScheduledPayoffs(schedule.sortWith(payoffs, calls), valuedate)

  def updateFutureFixings(vd: Date):Unit = scheduledPayoffs.foreach{case (cp, p, c) =>
    if (cp.eventDate le vd) {
      p.setPastFixing
      c.setPastFixing
    } else {
      p.setFutureFixing
      c.setFutureFixing
    }
  }

}


object ScheduledPayoffs {

  def empty:ScheduledPayoffs = ScheduledPayoffs(Schedule.empty, Payoffs.empty, Callabilities.empty)

  def allFixingUnderlyings(payoffs:Payoffs, calls:Callabilities) = payoffs.underlyings ++ calls.underlyings ++ payoffs.paymentFXUnderlyings

  def getFixingInformation(payoffs:Payoffs, calls:Callabilities):FixingInformation = payoffs.headOption match {
    case Some(p) => p.fixingInfo
    case _ => calls.headOption.collect{case c => c.fixingInfo}.getOrElse(FixingInformation.empty("JPY", "JPY"))
  }

  def apply(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val fixingMap = getFixings(allFixingUnderlyings(payoffs, calls), schedule.eventDates, getFixingInformation(payoffs, calls))
    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
    calls.assignAccumulatedPayments(schedule, payoffs)
    
    if (payoffs.exists(p => p.physical)) {
      val settlementFixingMap = getFixings(allFixingUnderlyings(payoffs, calls), schedule.paymentDates, getFixingInformation(payoffs, calls))
      payoffs.assignSettlementFixings(settlementFixingMap)
    }
      
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
    
  def sorted(schedule:Schedule, payoffs:Payoffs, calls:Callabilities, keepFinalLeg:Boolean = false):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    //val allUnderlyings = payoffs.underlyings ++ calls.underlyings
    val fixingMap = getFixings(allFixingUnderlyings(payoffs, calls), schedule.eventDates, getFixingInformation(payoffs, calls))
    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
    calls.assignAccumulatedPayments(schedule, payoffs)

    if (payoffs.exists(p => p.physical)) {
      val settlementFixingMap = getFixings(allFixingUnderlyings(payoffs, calls), schedule.paymentDates, getFixingInformation(payoffs, calls))
      payoffs.assignSettlementFixings(settlementFixingMap)
    }
    
    ScheduledPayoffs(schedule.sortWith(payoffs, calls, keepFinalLeg), None)
  }
  
  def getFixings(underlyings:Set[String], dates:List[Date], fixingInfo:FixingInformation):List[Map[String, Double]] = {
    DB.getFixings(underlyings, dates, fixingInfo) //.map(vs => vs.collect{case (ul, Some(v)) => (ul, v)})

//    val underlyingFixingInfos:Map[String, UnderlyingFixingInfo] = fixingInfo match {
//      case None => Map.empty
//      case Some(fixInfo) => underlyings.map(ul => (ul, fixInfo.getUnderlyingFixing(ul))).filter{case (ul, infos) => !infos.fixingPages.isEmpty}.toMap
//    }
//
//    val allFixingPages:Set[String] = underlyings ++ underlyingFixingInfos.map{case (ul, infos) => infos.fixingPages.map(p => p.pageList).flatten}.flatten.toSet
//
//    val pastFixings:List[Map[String, Option[Double]]] = DB.pastFixings(allFixingPages, dates)
//
//    val baseFixings:List[Map[String, Double]] = pastFixings.map(_.collect{case (k, Some(v)) => (k, v)})
//
//    val customFixings:List[Map[String, Double]] = baseFixings.map(fixingMap => {
//      underlyingFixingInfos.map {case (ul, infos) => (ul, infos.getPriceFromFixings(fixingMap))}.collect{case (ul, Some(v)) => (ul, v)}.toMap
//    })
//
//    baseFixings.zip(customFixings).map{case (b, c) => b ++ c}
  }
    
  def getExterpolatedFixings(underlyings:Set[String], dates:List[Date], fixingInfo:FixingInformation, valuedate:Date):List[Map[String, Double]] = {
    if (dates.isEmpty) {return List.empty}
    val valuedate = DB.latestParamDate.getOrElse(dates.max)
    val (datesbefore, datesafter) = dates.span(_ le valuedate)
    val fixings = getFixings(underlyings, datesbefore, fixingInfo)
    val currentFixings:List[Map[String, Double]] = DB.pastFixings(underlyings, List(valuedate)).map(_.collect{case (k, Some(v)) => (k, v)})
    val exterps:List[Map[String, Double]] = datesafter.map{case _ => 
      if (!currentFixings.isEmpty) currentFixings.last.map{case (k, v) => (k, v)}
      else if (!fixings.isEmpty) fixings.last.map{case (k, v) => (k, v)}
      else Map.empty[String, Double]}
    
    fixings ++ exterps
  }
  
  def extrapolate(schedule:Schedule, payoffs:Payoffs, calls:Callabilities, valuedate:Date, keepFinalLeg:Boolean = false):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val (datesbefore, datesafter) = schedule.eventDates.span(_ le valuedate)
    //val allUnderlyings = payoffs.underlyings ++ calls.underlyings

    val fixingMap:List[Map[String, Double]] = getExterpolatedFixings(
      allFixingUnderlyings(payoffs, calls),
      datesbefore,
      getFixingInformation(payoffs, calls),
      valuedate) ++ List.fill(datesafter.size)(Map.empty[String, Double])

    payoffs.assignFixings(fixingMap)
    calls.assignFixings(fixingMap)
//    if (calls.isTargetRedemption) {calls.assignAccumulatedPayments(schedule, payoffs)}
    calls.assignAccumulatedPayments(schedule, payoffs)
    
    if (payoffs.exists(p => p.physical)) {
      val (paymentDatesBefore, paymentDatesAfter) = schedule.paymentDates.span(_ le valuedate)

      val settlementFixingMap:List[Map[String, Double]] = getExterpolatedFixings(
        allFixingUnderlyings(payoffs, calls),
        paymentDatesBefore,
        getFixingInformation(payoffs, calls),
        valuedate) ++ List.fill(paymentDatesAfter.size)(Map.empty[String, Double])

      payoffs.assignSettlementFixings(settlementFixingMap)
    }
      
    
    ScheduledPayoffs(schedule.sortWith(payoffs, calls, keepFinalLeg), None)
  }
  
  def noFixing(schedule:Schedule, payoffs:Payoffs, calls:Callabilities):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
  
}