package net.squantlib.schedule

import scala.collection.LinearSeq
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.schedule.call.{Callabilities, Callability}
import net.squantlib.schedule.payoff.{FixedPayoff, Payoff, Payoffs}

import scala.collection.JavaConversions._
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import org.jquantlib.time.{BusinessDayConvention, Calendar}
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.daycounters._

import scala.runtime.ZippedTraversable3.zippedTraversable3ToTraversable

case class ScheduledPayoffs(
  scheduledPayoffs:LinearSeq[(CalculationPeriod, Payoff, Callability)],
  valuedate:Option[Date] = None
)(implicit val fixingInfo: FixingInformation) extends LinearSeq[(CalculationPeriod, Payoff, Callability)]{
  
  lazy val (schedule, payoffs, calls) = scheduledPayoffs.unzip3  match {
    case (s, p, c) => (Schedule(s), Payoffs(p), Callabilities(c))
  }

  lazy val coupon = Payoffs(payoffs.dropRight(1))

  lazy val redemption = payoffs.last

  scheduledPayoffs.foreach{case (s, p, c) =>
    if (p.isAbsolute) s.daycounter = new Absolute
    s.nominal = p.nominal
  }

  def isPriceable = payoffs.isPriceable && calls.isPriceable

  def trigCheckPayoff:ScheduledPayoffs = {
    val newcalls = calls.map(c => {
      if (c.isTrigger) 
        Callability(
          bermudan = false,
          triggers = c.triggers,
          triggerUp = c.triggerUp,
          targetRedemption = None,
          forward = UnderlyingFixing.empty, //c.forward,
          bonusAmount = 0.0, //c.bonusAmount,
          removeSatisfiedTriggers = c.removeSatisfiedTriggers,
          inputString = c.inputString,
          accumulatedPayments = None,
          simulatedFrontier= c.simulatedFrontier
        )(c.fixingInfo)

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
  
  lazy val terminationAmounts:List[BigDecimal] = calls.map(_.bonusAmount + 1.0).toList

  lazy val bonusAmounts:List[BigDecimal] = calls.map(_.bonusAmount).toList

  lazy val triggerUps:List[Boolean] = calls.map(_.triggerUp).toList

  lazy val forwardStrikes:List[Option[UnderlyingFixing]] = calls.map(c => if (c.forward.isEmpty) None else Some(c.forward)).toList
  
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
      case (d, p, t) if p.isPaymentFixed => List.empty
      case (d, p, t) => p.eventDates(d)
    }(collection.breakOut)

    valuedate match {
      case Some(d) => dates.map(ds => ds.filter(_ gt d))
      case None => dates
    }
  }

  var eventDateLegs:List[List[Date]] = computeEventDateLegs
  var eventDates:List[Date] = computeEventDates

  var dateMapper:List[List[Int]] = computeDateMapper
  def computeDateMapper:List[List[Int]] = {
    eventDateLegs.map(_.map(eventDates.indexOf(_)))
  }

  def computeEventDates:List[Date] = eventDateLegs.flatten.toSet.toList.sorted


  def computeCallEventDateLegs:List[Option[Date]] = {
    val dates: List[Option[Date]] = scheduledPayoffs.map {
      case (d, p, t) if t.isFixed => None
      case (d, p, t) => Some(d.callEventDate)
    }(collection.breakOut)

    valuedate match {
      case Some(d) => dates.map{
        case Some(td) => if (td gt d) Some(td) else None
        case None => None
      }
      case None => dates
    }
  }

  var callEventDateLegs:List[Option[Date]] = computeCallEventDateLegs
  var callEventDates:List[Date] = computeCallEventDates

  def computeCallEventDates:List[Date] = callEventDateLegs.collect{case Some(d) => d}

  def computeCallDateMapper:List[Option[Int]] = {
    callEventDateLegs.map(_.collect{case d => callEventDates.indexOf(d)})
  }

  var callDateMapper:List[Option[Int]] = computeCallDateMapper

  def allEventDates:List[Date] = (eventDates ++ callEventDates).toSet.toList.sorted

  def eventDateYears(basedate:Date):List[Double] = allEventDates.map(d => Date.daycount(basedate, d, defaultDaycounter))

  def clearFixings:Unit = {
    payoffs.foreach(_.clearFixings)
    calls.foreach(_.clearFixings)
    eventDateLegs = computeEventDateLegs
    eventDates = computeEventDates
    callEventDates = computeCallEventDates
    dateMapper = computeDateMapper
    callDateMapper = computeCallDateMapper
  }

  abstract class withDefault[T] { def defaultValue:T }
    
  implicit object mapValue extends withDefault[Map[String, Double]] { def defaultValue = Map.empty[String, Double]}

  implicit object listValue extends withDefault[List[Double]] { def defaultValue = List.fill(underlyings.size)(Double.NaN)}

  implicit object underlyingFixingValue extends withDefault[UnderlyingFixing] { def defaultValue = UnderlyingFixing.errorValue(underlyings.toSet)}

  implicit object doubleValue extends withDefault[Double] { def defaultValue = Double.NaN}

  def priceMapper[T](fixings:List[T])(implicit defclass:withDefault[T]):List[List[T]] = {
    dateMapper.map(ds => {
      if (ds.isEmpty) List(defclass.defaultValue) else ds.map(fixings)
    })
  }

  def callPriceMapper[T](fixings:List[T])(implicit defclass:withDefault[T]):List[Option[T]] = {
    callDateMapper.map(_ match {
      case Some(d) => Some(fixings(d))
      case _ => None
    })
  }

  //  def priceMapper(fixings:List[UnderlyingFixing]):List[List[UnderlyingFixing]] = dateMapper.map(d => {
//    if (d.isEmpty) List(UnderlyingFixing.errorValue(underlyings)) else d.map(fixings) })

//  def fixingPrices(fixings:List[Double]):List[List[Double]] = priceMapper(fixings)

  def fixingPrices(
    underlyingId:String,
    fixings:List[Double]
  ):List[List[Double]] = {
    priceMapper(fixings)
  }

  def fixingPrices(
    fixings:List[Map[String, Double]]
  ):List[List[Map[String, Double]]] = {
    priceMapper(fixings)
  }

  def fixingPrices(
    fixings:List[UnderlyingFixing]
  )(implicit d:DummyImplicit):List[List[UnderlyingFixing]] = {
    priceMapper(fixings)
  }

  def price:List[Double] = {
    if (calls.isTrigger) List.fill(payoffs.size)(Double.NaN)
    else payoffs.price
  }

  // Single Underlying
//  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = singleUnderlying match {
//    case Some(ul) => price(fixings.map(f => UnderlyingFixing(Map(ul -> f))))
//    case _ => List.fill(payoffs.size)(Double.NaN)
//  }

  // multiple underlyings
  def price(
    fixings:List[UnderlyingFixing]
 ):List[Double] = {
    payoffs.price(
      fixingList = priceMapper(fixings),
      callFixingList = callPriceMapper(fixings),
      calls = calls.toList,
      dayCounts = schedule.dayCounts,
      accruedPayment = None
    )
  }

  def price(
    fixings:List[Map[String, Double]]
  )(implicit dummyImplicit:DummyImplicit):List[Double] = {
    price(fixings.map(f => UnderlyingFixing(f)))
  }

  def price(
    underlyingId:String,
    fixings:List[Double]
  ):List[Double] = {
    price(fixings.map(f => UnderlyingFixing(Map(underlyingId -> f))))
  }


  def withValueDate(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs, Some(vd))
    
  private def filterAfter(vd:Date):CalculationPeriod => Boolean = cp => cp.paymentDate gt vd
  
  def after(vd:Date):ScheduledPayoffs = ScheduledPayoffs(scheduledPayoffs.filter{case (cp, _, _) => filterAfter(vd)(cp)})

  def afterWithValueDate(vd:Date) = {
    val newSchedule = scheduledPayoffs.filter{case (cp, _, _) => filterAfter(vd)(cp)}
    ScheduledPayoffs(newSchedule, Some(vd))
  }

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

  def called(
    vd:Date,
    currencyId:String,
    paymentCurrencyId:String,
    redemAmount:Double,
    paymentCalendar:Calendar,
    convention:BusinessDayConvention
  ):ScheduledPayoffs = {
    before(vd).addCashflow(vd, currencyId, paymentCurrencyId, redemAmount, paymentCalendar, convention, true)
  }
  
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

    val scheduleDescription = scheduledPayoffs.map{case (d, p, c) =>
      List(
        d.eventDate.toString,
        d.paymentDate.toString,
        p.toString,
        c.toString,
        if (underlyings.isEmpty || (p.variables.isEmpty && c.variables.isEmpty)) "fixed"
        else if (!p.isPaymentFixed && !c.isFixed) "not fixed"
        else "coupon:" + p.getFixings + " call:" +  c.getFixings
      )
    }.toList

    (title, scheduleDescription)
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

  var currentFutureFixingBaseDate:Option[Date] = None

  def updateFutureFixings(vd: Date):Unit = {
    if (currentFutureFixingBaseDate != Some(vd)) {
      scheduledPayoffs.foreach { case (cp, p, c) =>

        if (cp.eventDate le vd) {
          p.setPastFixing
          c.setPastFixing
        } else {
          p.setFutureFixing
          c.setFutureFixing
        }
      }
      currentFutureFixingBaseDate = Some(vd)
    }
  }

  def assignFixings:Unit = {

    val fixingMap:Map[Date, UnderlyingFixing] = ScheduledPayoffs.getFixings(
      ScheduledPayoffs.allFixingUnderlyings(payoffs, calls),
      (schedule.eventDates ++ schedule.callEventDates).toSet,
      ScheduledPayoffs.getFixingInformation(payoffs, calls),
      false
    )

    payoffs.assignFixings(schedule.eventDates.map(fixingMap))
    calls.assignFixings(schedule.callEventDates.map(fixingMap))
    calls.assignAccumulatedPayments(schedule, payoffs)

    if (payoffs.exists(p => p.physical)) {
      val settlementFixingMap:Map[Date, UnderlyingFixing] = ScheduledPayoffs.getFixings(
        ScheduledPayoffs.allFixingUnderlyings(payoffs, calls),
        schedule.paymentDates.toSet,
        ScheduledPayoffs.getFixingInformation(payoffs, calls),
        false
      )

      payoffs.assignSettlementFixings(schedule.paymentDates.map(settlementFixingMap))
    }
  }


}


object ScheduledPayoffs {

  def empty()(implicit fixingInfo: FixingInformation):ScheduledPayoffs = ScheduledPayoffs(Schedule.empty, Payoffs.empty, Callabilities.empty)

  def allFixingUnderlyings(payoffs:Payoffs, calls:Callabilities) = payoffs.underlyings ++ calls.underlyings ++ payoffs.paymentFXUnderlyings

  def getFixingInformation(payoffs:Payoffs, calls:Callabilities):FixingInformation = payoffs.headOption match {
    case Some(p) => p.fixingInfo
    case _ => calls.headOption.collect{case c => c.fixingInfo}.getOrElse(FixingInformation.empty("JPY", "JPY"))
  }

  def apply(
    schedule:Schedule,
    payoffs:Payoffs,
    calls:Callabilities
  )(implicit fixingInfo: FixingInformation):ScheduledPayoffs = {

    require (schedule.size == payoffs.size && schedule.size == calls.size)

    val fixingMap:Map[Date, UnderlyingFixing] = getFixings(
      allFixingUnderlyings(payoffs, calls),
      (schedule.eventDates ++ schedule.callEventDates).toSet,
      getFixingInformation(payoffs, calls),
      false
    )

    payoffs.assignFixings(schedule.eventDates.map(fixingMap))
    calls.assignFixings(schedule.callEventDates.map(fixingMap))
    calls.assignAccumulatedPayments(schedule, payoffs)

    if (payoffs.exists(p => p.physical)) {
      val settlementFixingMap = getFixings(
        allFixingUnderlyings(payoffs, calls),
        schedule.paymentDates.toSet,
        getFixingInformation(payoffs, calls),
        false
      )
      payoffs.assignSettlementFixings(schedule.paymentDates.map(d => settlementFixingMap(d)))
    }
      
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
    
  def sorted(
    schedule:Schedule,
    payoffs:Payoffs,
    calls:Callabilities,
    keepFinalLeg:Boolean = false
  )(implicit fixingInfo: FixingInformation):ScheduledPayoffs = {

    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val fixingMap:Map[Date, UnderlyingFixing] = getFixings(
      allFixingUnderlyings(payoffs, calls),
      (schedule.eventDates ++ schedule.callEventDates).toSet,
      getFixingInformation(payoffs, calls),
      false
    )

    payoffs.assignFixings(schedule.eventDates.map(fixingMap))
    calls.assignFixings(schedule.callEventDates.map(fixingMap))
    calls.assignAccumulatedPayments(schedule, payoffs)

    if (payoffs.exists(p => p.physical)) {
      val settlementFixingMap = getFixings(
        allFixingUnderlyings(payoffs, calls),
        schedule.paymentDates.toSet,
        getFixingInformation(payoffs, calls),
        false
      )
      payoffs.assignSettlementFixings(schedule.paymentDates.map(settlementFixingMap))
    }

    ScheduledPayoffs(schedule.sortWith(payoffs, calls, keepFinalLeg), None)
  }
  
  def getFixings(
    underlyings:Set[String],
    dates:Set[Date],
    fixingInfo:FixingInformation,
    isInitialFixing:Boolean
  ):Map[Date, UnderlyingFixing] = {
    val dateList = dates.toList.sorted
    val dbFixings:List[Map[String, Double]] = DB.getFixings(underlyings, dateList, fixingInfo, isInitialFixing)
    (dateList, dbFixings).zipped.map{case (d, vs) => (d, UnderlyingFixing(vs)(fixingInfo))}.toMap
  }
    
  def getExterpolatedFixings(
    underlyings:Set[String],
    dates:Set[Date],
    fixingInfo:FixingInformation,
    valuedate:Date
  ):Map[Date, UnderlyingFixing] = {

    if (dates.isEmpty) {return Map.empty}
    val valuedate = DB.latestParamDate.getOrElse(dates.max)
    val (datesBefore, datesAfter) = dates.span(_ le valuedate)

    val pastFixings:Map[Date, UnderlyingFixing] = getFixings(underlyings, datesBefore, fixingInfo, false)

    val currentFixings:Option[UnderlyingFixing] = DB.pastFixings(underlyings, List(valuedate))
      .headOption
      .collect{case vs => UnderlyingFixing(vs.collect{case (k, Some(v)) => (k, v)})(fixingInfo)}

    val exterpolatedFixings:Map[Date, UnderlyingFixing] = datesAfter.map{case dafter =>
      (dafter, currentFixings.getOrElse(if (!pastFixings.isEmpty) UnderlyingFixing.empty else pastFixings.maxBy{case (d, _) => d}._2))
    }.toMap
    
    pastFixings ++ exterpolatedFixings
  }
  
  def extrapolate(
    schedule:Schedule,
    payoffs:Payoffs,
    calls:Callabilities,
    valuedate:Date,
    keepFinalLeg:Boolean = false
  )(implicit fixingInfo: FixingInformation):ScheduledPayoffs = {

    require (schedule.size == payoffs.size && schedule.size == calls.size)
    val (datesBefore, datesAfter) = (schedule.eventDates ++ schedule.callEventDates).toSet.span(_ le valuedate)

    val fixingMap:Map[Date, UnderlyingFixing] = {
      getExterpolatedFixings(
        allFixingUnderlyings(payoffs, calls),
        datesBefore,
        getFixingInformation(payoffs, calls),
        valuedate
      ) ++ datesAfter.map(d => (d, UnderlyingFixing.empty)).toMap
    }

    payoffs.assignFixings(schedule.eventDates.map(fixingMap))
    calls.assignFixings(schedule.callEventDates.map(fixingMap))
    calls.assignAccumulatedPayments(schedule, payoffs)
    
    if (payoffs.exists(p => p.physical)) {
      val (paymentDatesBefore, paymentDatesAfter) = schedule.paymentDates.toSet.span(_ le valuedate)

      val settlementFixingMap:Map[Date, UnderlyingFixing] = {
        getExterpolatedFixings(
          allFixingUnderlyings(payoffs, calls),
          paymentDatesBefore,
          getFixingInformation(payoffs, calls),
          valuedate
        ) ++ paymentDatesAfter.map(d => (d, UnderlyingFixing.empty)).toMap
      }

      payoffs.assignSettlementFixings(schedule.paymentDates.map(settlementFixingMap))
    }
      
    
    ScheduledPayoffs(schedule.sortWith(payoffs, calls, keepFinalLeg), None)
  }
  
  def noFixing(
    schedule:Schedule,
    payoffs:Payoffs,
    calls:Callabilities
  )(implicit fixingInfo: FixingInformation):ScheduledPayoffs = {
    require (schedule.size == payoffs.size && schedule.size == calls.size)
    ScheduledPayoffs((schedule, payoffs, calls).zipped.toList, None)
  }
  
}