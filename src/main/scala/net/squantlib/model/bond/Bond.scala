package net.squantlib.model.bond

import net.squantlib.util.Date
import net.squantlib.database.DB
import net.squantlib.database.schemadefinitions.{Bond => dbBond}
import net.squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import net.squantlib.model.asset.{AnalyzedAsset, Underlying}
import net.squantlib.util.{SimpleCache, FormulaParser}
import net.squantlib.pricing.model.PricingModel
import net.squantlib.schedule.call.Callabilities
import net.squantlib.schedule.payoff.{Payoff, Payoffs}
import net.squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import net.squantlib.model.market.Market
import net.squantlib.pricing.model.NoModel
import org.jquantlib.currencies.Currency
import net.squantlib.util.FixingInformation
import net.squantlib.util.DisplayUtils._

trait AnalyzableBond 
	extends PriceableBond 
    with YieldAnalysis
    with GreekAnalysis
    with BondAsset {
  
  override val db:dbBond 
    
  override val scheduledPayoffs:ScheduledPayoffs
    
  override val underlyings:List[String]
  
  override val assetStartDate = Some(issueDate)
  
  override val assetEndDate = Some(terminationDate)
  
  override def isPriced: Boolean = dirtyPrice.isDefined
  
  override def latestPriceLocalCcy: Option[Double] = dirtyPrice
  
  override def latestPrice:Option[Double] = dirtyPrice
  
  override def expectedYield:Option[Double] = yieldContinuous
  
  override def expectedCoupon:Option[Double] = currentRate
  
  override def latestPriceDate:Option[Date] = valueDate
  
}


case class Bond(
  override val db:dbBond, 
  override val scheduledPayoffs:ScheduledPayoffs,
	override val underlyings:List[String]) 
	extends AnalyzableBond  {
  
  override def copy:Bond = copy(db, scheduledPayoffs, underlyings)
  override def copy(newdb:dbBond, newSchedule:ScheduledPayoffs, newuls:List[String]):Bond = {
    val bond = Bond(newdb, newSchedule, newuls)
    super.transferSettings(bond)
    bond
  }
  
}

object Bond {
  
  def apply(db:dbBond):Option[Bond] = getScheduledPayoffs(db, None, true).collect{case po => new Bond(db, po, db.underlyingList)}
  
  def apply(db:dbBond, valuedate:Option[Date], pastFixings:Boolean, customFixings:FixingInformation):Option[Bond] = getScheduledPayoffs(db, valuedate, pastFixings, customFixings).collect{case po => new Bond(db, po, db.underlyingList)}
  
  def forward(db:dbBond, valuedate:Date):Option[Bond] = getScheduledPayoffs(db, Some(valuedate), true).collect{case po => new Bond(db, po, db.underlyingList)}
  
  def noFixings(db:dbBond):Option[Bond] = getScheduledPayoffs(db, None, false).collect{case po => new Bond(db, po, db.underlyingList)}

  def getScheduledPayoffs(db:dbBond, valuedate:Option[Date] = None, assignPastFixings:Boolean = true, customFixings:FixingInformation = null):Option[ScheduledPayoffs] = {
    val schedule = db.schedule.orNull
    if (schedule == null) {return None}
    
    implicit val fixingInfo:FixingInformation = if (customFixings == null) db.fixingInformation else customFixings
    
    val coupon:Payoffs = Payoffs(db.coupon, schedule.size - 1).orNull
    if (coupon == null || coupon.size + 1 != schedule.size) {errorOutput(db.id, "cannot initialize coupon"); return None}
      
    val redemption = Payoff(db.redemprice).orNull
    if (redemption == null) {errorOutput(db.id, "cannot initialize redemption"); return None}
      
    val underlyings:List[String] = db.underlyingList
      
    val calls = Callabilities(db.call, underlyings, schedule.size)
    if (calls == null) {errorOutput(db.id, "cannot initialize calls"); return None}
      
    val scheduledPayoffs = valuedate match {
      case Some(d) => ScheduledPayoffs.extrapolate(schedule, coupon :+ redemption, calls.fill(schedule.size), d)
      case None if assignPastFixings => ScheduledPayoffs.sorted(schedule, coupon :+ redemption, calls.fill(schedule.size))
      case None => ScheduledPayoffs.noFixing(schedule, coupon :+ redemption, calls.fill(schedule.size))
    }
    
    if (scheduledPayoffs == null || scheduledPayoffs.isEmpty) {
      errorOutput(db.id, "cannot initialize scheduled payoffs")
      None
    } else Some(scheduledPayoffs)
  }
 
}

