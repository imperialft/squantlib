package squantlib.model.bond

import org.jquantlib.currencies.Currency
import squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod, TimeUnit, Calendar, Frequency}
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.util.initializer.Currencies
import squantlib.schedule.call.Callabilities
import squantlib.schedule.payoff.{Payoff, Payoffs}
import squantlib.schedule.{ScheduledPayoffs, Schedule, CalculationPeriod}
import org.codehaus.jackson.JsonNode
import squantlib.util.UnderlyingParsers


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
  
  def trigger:List[List[Option[Double]]] = calls.triggerValues(underlyings)
  
  def nominal:Option[Double] = db.nominal
  
  val currency:Currency = db.currency.get
  
  def denomination:Option[Double] = db.denomination
  
  def period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

  val calendar:Calendar = db.calendar
  
  def issuePrice:Option[Double] = db.issueprice
  
  def call:String = db.call
  
  def initialFX:Double = db.initialfx
  
  def issuer:String = db.issuerid
  
  def settings:JsonNode = db.settingsJson
  
  def isFixedRateBond = payoffs.underlyings.size == 0
  
  def redemption:Payoff = {
    val abslegs = scheduledPayoffs.filter{case (s, p, t) => s.isAbsolute}
    val finalleg = if (abslegs.size > 0) abslegs.maxBy{case (s, p, t) => s.paymentDate}
            else scheduledPayoffs.maxBy{case (s, p, t) => s.paymentDate}
    finalleg._2
  }
  
  def fixedRedemptionAmount:Option[Double] = {
    val amount = redemption.price
    if (amount.isNaN || amount.isInfinity) None else Some(amount)
  }
  
  def isScheduleMaturedOn(d:Date):Boolean = d ge scheduledMaturity
  
  lazy val (earlyTerminationPeriod:Option[CalculationPeriod], earlyTerminationAmount:Option[Double]) = 
    scheduledPayoffs.triggeredDate.collect{case (p, a) => (Some(p), Some(a))}.getOrElse((None, None))
  
  lazy val earlyTerminationDate:Option[Date] = earlyTerminationPeriod.collect{case p => p.paymentDate}
  
  def isEarlyTerminatedOn(d:Date):Boolean = earlyTerminationDate.collect{case dd => d ge dd}.getOrElse(false)
    
  def isTerminatedOn(d:Date):Boolean = isScheduleMaturedOn(d) || isEarlyTerminatedOn(d)
    
  def terminationDate:Date = earlyTerminationDate.getOrElse(scheduledMaturity)
  
  def currencyList:Set[String] = underlyings.map(UnderlyingParsers.extractCurrencies).flatten.toSet + currency.code
    
	
} 


