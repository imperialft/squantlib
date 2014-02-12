package squantlib.model.bond

import squantlib.util.Date
import squantlib.database.DB
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import squantlib.model.asset.{AnalyzedAsset, Underlying}
import squantlib.util.{SimpleCache, FormulaParser}
import squantlib.pricing.model.PricingModel
import squantlib.schedule.call.Callabilities
import squantlib.schedule.payoff.{Payoff, Payoffs}
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import squantlib.model.market.Market
import squantlib.pricing.model.NoModel
import org.jquantlib.currencies.Currency

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
  
  def apply(db:dbBond):Option[Bond] = 
    getScheduledPayoffs(db).collect{case po => new Bond(db, po, db.underlyingList)}
  
  def forward(db:dbBond, valuedate:Date):Option[Bond] = 
    getScheduledPayoffs(db, Some(valuedate)).collect{case po => new Bond(db, po, db.underlyingList)}
  

  def getScheduledPayoffs(db:dbBond, valuedate:Option[Date] = None):Option[ScheduledPayoffs] = {
	val schedule = db.schedule.orNull
	if (schedule == null) {return None}
	
	implicit val fixingInfo = db.fixingInformation
	
	val coupon:Payoffs = Payoffs(db.coupon, schedule.size - 1).orNull
	if (coupon == null || coupon.size + 1 != schedule.size) {println(db.id + ": cannot initialize coupon"); return None}
	  
	val redemption = Payoff(db.redemprice).orNull
	if (redemption == null) {println(db.id + ": cannot initialize redemption"); return None}
	  
	val underlyings:List[String] = db.underlyingList
		
	val bermudan:List[Boolean] = db.bermudanList(schedule.size) match {
	  case berms if !berms.isEmpty && berms.takeRight(1).head => berms.dropRight(1) :+ false
	  case berms => berms
	}
	
	val trigger = db.triggerList(schedule.size)
	  
	val calls = Callabilities(bermudan, trigger, underlyings)
	if (calls == null) {println(db.id + ": cannot initialize calls"); return None}
	  
	val scheduledPayoffs = valuedate match {
	  case Some(d) => ScheduledPayoffs.extrapolate(schedule, coupon :+ redemption, calls.fill(schedule.size), d)
	  case None => ScheduledPayoffs.sorted(schedule, coupon :+ redemption, calls.fill(schedule.size))
	}
	
	if (scheduledPayoffs == null || scheduledPayoffs.isEmpty) {
	  println(db.id + ": cannot initialize scheduled payoffs"); None}
	else Some(scheduledPayoffs)
  }
 
}

