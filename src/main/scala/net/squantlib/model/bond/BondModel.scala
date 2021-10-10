package net.squantlib.model.bond

import net.squantlib.util.ql.currencies.Currency
import net.squantlib.util.{Date, DbCalendar}
import net.squantlib.util.ql.time.{Period => qlPeriod, TimeUnit}
import net.squantlib.database.schemadefinitions.{Bond => dbBond}
import net.squantlib.util.FixingInformation
import net.squantlib.util.initializer.Currencies
import net.squantlib.schedule.call.Callabilities
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.schedule.{ScheduledPayoffs, Schedule, CalculationPeriod}
import net.squantlib.util.UnderlyingParsers
import com.fasterxml.jackson.databind.JsonNode


trait BondModel {

  val db:dbBond
  
  val scheduledPayoffs:ScheduledPayoffs
  
  val underlyings:List[String]
  
  def copy:BondModel
  
  def schedule:Schedule = scheduledPayoffs.schedule
  
  def payoffs:Payoffs = scheduledPayoffs.payoffs
  
  def calls:Callabilities = scheduledPayoffs.calls
  
  def coupon = scheduledPayoffs.coupon
  
  val id:String = db.id
  
  def issueDate:Date = schedule.head.startDate
  
  def isIssuedOn(d:Date):Boolean = d ge issueDate
  
  def scheduledMaturity:Date = schedule.last.endDate
  
  def bermudan:List[Boolean] = calls.bermudans
  
  def trigger:List[List[Option[BigDecimal]]] = calls.triggerValues(underlyings)

  def targetRedemptions:List[Option[BigDecimal]] = calls.targetRedemptions

  def nominal:Option[BigDecimal] = db.nominal
  
  def currency:Currency = db.currency
  
  def denomination:Option[BigDecimal] = db.denomination
  
  def period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

  val fixingCalendar:DbCalendar = db.fixingCalendar

  val paymentCalendar:DbCalendar = db.paymentCalendar
  
  def issuePrice:Option[BigDecimal] = db.issueprice
  
  def call:String = db.call
  
  def initialFX:BigDecimal = db.initialfx
  
  def issuer:String = db.issuerid
  
  def settings:JsonNode = db.settingsJson
  
  def isFixedRateBond = payoffs.underlyings.size == 0
  
  def redemption:Payoff = {
    val abslegs = scheduledPayoffs.filter{case (s, p, t) => s.isRedemption}
    val finalleg = 
      if (abslegs.size > 0) abslegs.maxBy{case (s, p, t) => s.getPaymentDate}
      else scheduledPayoffs.maxBy{case (s, p, t) => s.getPaymentDate}
    finalleg._2
  }
  
  def fixedRedemptionAmount:Option[Double] = {
    val amount = redemption.price
    if (amount.isNaN || amount.isInfinity) None else Some(amount)
  }
  
  def isScheduleMaturedOn(d:Date):Boolean = d ge scheduledMaturity

  def earlyTerminationPeriod:Option[CalculationPeriod] = {
    if (earlyTerminationPeriodFixed == null) {
      initializeEarlyTermination
    }
    earlyTerminationPeriodFixed
  }

  def earlyTerminationAmount:Option[Double] = {
    if (earlyTerminationAmountFixed == null) {
      initializeEarlyTermination
    }
    earlyTerminationAmountFixed
  }

  var earlyTerminationPeriodFixed:Option[CalculationPeriod] = null
  var earlyTerminationAmountFixed:Option[Double] = null

  def initializeEarlyTermination = {
    val (v1, v2) = scheduledPayoffs.calledDate.collect { case (p, a) => (Some(p), Some(a)) }.getOrElse((None, None))
    earlyTerminationPeriodFixed = v1
    earlyTerminationAmountFixed = v2
  }
  
  lazy val earlyTerminationDate:Option[Date] = earlyTerminationPeriod.collect{case p => p.getCallValueDate}
  
  def isEarlyTerminatedOn(d:Date):Boolean = earlyTerminationDate.collect{case dd => d ge dd}.getOrElse(false)
    
  def isTerminatedOn(d:Date):Boolean = isScheduleMaturedOn(d) || isEarlyTerminatedOn(d)
    
  def terminationDate:Date = earlyTerminationDate.getOrElse(scheduledMaturity)
  
  def currencyList:Set[String] = underlyings.map(UnderlyingParsers.extractCurrencies).flatten.toSet + currency.code
  
  implicit var fixingInformation:FixingInformation = db.fixingInformation
	
} 


