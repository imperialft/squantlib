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
import net.squantlib.util.ql.currencies.Currency
import net.squantlib.util.FixingInformation
import net.squantlib.util.DisplayUtils._
import scala.collection.LinearSeq
import net.squantlib.util.JsonUtils._
import com.fasterxml.jackson.databind.JsonNode


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
  
  // override def latestPriceLocalCcy: Option[Double] = dirtyPrice
  
  override def latestPrice:Option[Double] = dirtyPrice
  
  // override def expectedYield:Option[Double] = yieldContinuous
  
  // override def expectedCoupon:Option[Double] = currentRate
  
  // override def latestPriceDate:Option[Date] = valueDate
  
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

  def getScheduledPayoffs(
    db:dbBond,
    valuedate:Option[Date] = None,
    assignPastFixings:Boolean = true,
    customFixings:FixingInformation = null
  ):Option[ScheduledPayoffs] = {
    val schedule = db.schedule.orNull
    if (schedule == null) {return None}
    
    implicit val fixingInfo:FixingInformation = {
      if (customFixings == null) db.fixingInformation
      else customFixings
    }

    val coupon:Payoffs = Payoffs(db.coupon, schedule.size - 1).orNull
    if (coupon == null || coupon.size + 1 != schedule.size) {
      errorOutput(db.id, "cannot initialize coupon")
      return None
    }

    val redemption = Payoff(db.redemprice).orNull
    if (redemption == null) {errorOutput(db.id, "cannot initialize redemption"); return None}
    redemption.setAbsolute

    val underlyings:List[String] = db.underlyingList
      
    val calls = db.call.jsonNode match {
      case Some(formulaJson) if formulaJson.isArray =>

        val finalCall:Option[JsonNode] = db.redemprice.jsonNode match {
          case Some(r) if r.isObject => r.getOption("callability")
          case _ => None
        }

        Callabilities(formulaJson, finalCall, underlyings, schedule.size)
      case _ => Callabilities.empty(schedule.size)
    }

    if (calls == null) {errorOutput(db.id, "cannot initialize calls"); return None}

    var scheduledPayoffs = valuedate match {
      case Some(d) => ScheduledPayoffs.extrapolate(schedule, coupon :+ redemption, calls.fill(schedule.size), d, true)
      case None if assignPastFixings => ScheduledPayoffs.sorted(schedule, coupon :+ redemption, calls.fill(schedule.size), true)
      case None => ScheduledPayoffs.noFixing(schedule, coupon :+ redemption, calls.fill(schedule.size))
    }

    val newLegs:LinearSeq[Payoff] = scheduledPayoffs.scheduledPayoffs.dropRight(1).map{case (s, p, c) =>
      p.getRedemption match {
        case Some(r) =>
          scheduledPayoffs = scheduledPayoffs.insert(s.getRedemptionLeg(p.nominal), r, c)
          r.setAbsolute
          Some(r)
        case _ => None
      }
    }.flatMap(s => s)

    if (!newLegs.isEmpty) {
      scheduledPayoffs.assignFixings
    }

//    db.manualTerminationDate.collect{case d =>
//      scheduledPayoffs.foreach{ case (s, p, c) =>
//        if (s.paymentDate == d) c.setIssuerCalled(true)
//      }
//    }

    if (scheduledPayoffs == null || scheduledPayoffs.isEmpty) {
      errorOutput(db.id, "cannot initialize scheduled payoffs")
      None
    } else Some(scheduledPayoffs)
  }
 
}

