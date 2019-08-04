package net.squantlib.model.bond

import net.squantlib.util.Date
import org.jquantlib.daycounters.{Actual365Fixed, DayCounter}
import net.squantlib.util.SimpleCache
import net.squantlib.pricing.model.PricingModel
import net.squantlib.schedule.call.{Callability, Callabilities}
import net.squantlib.schedule.{CalculationPeriod, ScheduledPayoffs}
import net.squantlib.schedule.payoff.Payoff
import net.squantlib.model.market.Market
import net.squantlib.database.DB

import scala.annotation.tailrec

/**
 * Bond class with enclosed risk analysis functions.
 */ 

trait ExtendedSchedule {
  
  self : BondModel =>
  
  def market:Option[Market]
  
  def valueDate:Option[Date] = market.collect{case mkt => mkt.valuedate}
  
  val cache:SimpleCache = null
  
  def isIssued:Option[Boolean] = valueDate.collect{case d => isIssuedOn(d)}
  
  def isScheduleMatured:Option[Boolean] = valueDate.collect { case vd => isScheduleMaturedOn(vd)}

  def isEarlyTerminated:Option[Boolean] = valueDate.collect{case d => isEarlyTerminatedOn(d)}
  
  def isTerminated:Option[Boolean] = valueDate.collect{case d => isScheduleMaturedOn(d) || isEarlyTerminatedOn(d)}
  
  /*
   * Remaining life in number of years
   */
  def remainingLife:Option[Double] = valueDate.collect{ case d => Date.daycount(d, terminationDate, new Actual365Fixed).max(0.0)}
  
  /*  
   * Returns "live" payment schedules
   *   @returns element 1: Schedule containing legs with payment date after market value date or specified value date.
   *       element 2: Payoffs containing legs with payment dates after market value date or specified value date.
   */
  def livePayoffs:ScheduledPayoffs = valueDate.collect {case d => livePayoffs(d)}.getOrElse(ScheduledPayoffs.empty)

  def livePayoffs(vd:Date):ScheduledPayoffs = {
    val p = (earlyTerminationDate,earlyTerminationAmount) match {
      case (Some(d), _) if vd ge d => ScheduledPayoffs.empty
      case (Some(d), Some(a)) => scheduledPayoffs.after(vd).called(d, db.currencyid, db.paymentCurrencyId, a, db.paymentCalendar, db.paymentAdjust).withValueDate(vd)
      case _ => scheduledPayoffs.after(vd).withValueDate(vd)
    }
    p
  }

  def livePayoffCount(vd:Date):Int =
    earlyTerminationDate match {
      case Some(d) if vd ge d => 0
      case Some(d) => scheduledPayoffs.countBetween(vd, d) + 1
      case _ => scheduledPayoffs.countAfter(vd)
    }
  
  def liveCoupons:ScheduledPayoffs = livePayoffs.filtered{case (period, _, _) => !period.isRedemption}
  
  def liveCoupons(vd:Date):ScheduledPayoffs = livePayoffs(vd).filtered{case (period, _, _) => !period.isRedemption}
  
  /*  
   * Returns "live" triggers
   *   @returns list of remaining triggers
   */
  
  def liveCallabilities:Callabilities = livePayoffs.calls
  
  def liveTriggers:List[List[Option[BigDecimal]]] = livePayoffs.calls.triggerValues(underlyings)
  
  def liveTriggers(vd:Date):List[List[Option[BigDecimal]]] = livePayoffs(vd).calls.triggerValues(underlyings)
  
  /*  
   * Returns "live" bermudan call options
   *   @returns list of remaining bermudan calls
   */
  def liveBermudans:List[(CalculationPeriod, Boolean)] = livePayoffs.map{case (d, _, c) => (d, c.isBermuda)} (collection.breakOut)
    
  def liveBermudans(vd:Date):List[(CalculationPeriod, Boolean)] = livePayoffs(vd).map{case (d, _, c) => (d, c.isBermuda)}(collection.breakOut)

  /*  
   * Returns coupons fixed with current spot market (not forward!). 
   */


  @tailrec private def spotFixedRateRec(remainSchedule: List[(CalculationPeriod, Payoff, Callability)], acc: List[(CalculationPeriod, Double)]): List[(CalculationPeriod, Double)] = (remainSchedule.headOption, market) match {
    case (None, _) => acc.reverse
    case (Some((d, p, c)), Some(mkt)) => spotFixedRateRec(remainSchedule.tail, (d, p.price(mkt, acc.map{case (d, v) => v})) :: acc)
    case (Some((d, p, c)), None) => spotFixedRateRec(remainSchedule.tail, (d, Double.NaN) :: acc)
  }

  def spotFixedRates:List[(CalculationPeriod, Double)] = {
    //    def retrieverfunc:List[(CalculationPeriod, Double)] =
    //      livePayoffs.map{case (d, p, _) =>
    //        (d, market match {
    //          case Some(mkt) => p.price(mkt)
    //          case None => Double.NaN})}(collection.breakOut)

    if (cache == null) spotFixedRateRec(livePayoffs.toList, List.empty)
    else cache.getOrUpdate("SPOTFIXEDRATES", spotFixedRateRec(livePayoffs.toList, List.empty))
  }
    
  def spotFixedRates(vd:Date):List[(CalculationPeriod, Double)] = spotFixedRates.filter{case (p, d) => (p.paymentDate gt vd)}
    
  def spotFixedAmount:List[(Date, Double)] = spotFixedRates.map{case (period, rate) => (period.paymentDate, rate * period.dayCount * period.nominal)}
  
  def spotFixedAmount(vd:Date):List[(Date, Double)] = spotFixedAmount.filter{case (d, _) => (d gt vd)}
    
  def spotFixedRatesAll:List[(CalculationPeriod, Double)] = cache.getOrUpdate("SPOTFIXEDRATESALL",
      spotFixedRateRec(scheduledPayoffs.toList, List.empty)
//      scheduledPayoffs.map{case (d, p, _) =>
//        (d, market.collect{case mkt => p.price(mkt)}.getOrElse(Double.NaN))} (collection.breakOut)
  )
    
  def spotFixedAmountAll:List[(Date, Double)] = spotFixedRatesAll.map{case (period, rate) => (period.paymentDate, rate * period.dayCount * period.nominal)}
  
  def spotCashflowDayfrac(dc:DayCounter):List[(Double, Double)] = spotFixedAmount.map{
    case (payday, amount) => (Date.daycount(valueDate.get, payday, dc), amount)}
  
  def spotCashflowDayfracAll(dc:DayCounter):List[(Double, Double)] = spotFixedAmountAll.map{
    case (payday, amount) => (Date.daycount(issueDate, payday, dc), amount)}
  
  def spotCashflowDayfracAllJpy(dc:DayCounter):List[(Double, Double)] = {
    val current:Double = DB.getLatestPrice(currency.code + "JPY").collect{case v => v._2}.getOrElse(Double.NaN)
    val initfx:BigDecimal = if (initialFX > 0.0) initialFX else current
    val fxs:List[Double] = fixedFx.map(_.getOrElse(current))
    (spotCashflowDayfracAll(dc), fxs).zipped.map{case ((d, p), fx) => (d, p * fx / initfx.toDouble)}
  }
  
  val fixedFx:List[Option[Double]] = currency match {
    case ccy if ccy.code == "JPY" => List.fill(scheduledPayoffs.size)(Some(1.0))
    case ccy => DB.pastFixing(ccy.code + "JPY", scheduledPayoffs.map(_._1.paymentDate).toList)
  }
  
}

