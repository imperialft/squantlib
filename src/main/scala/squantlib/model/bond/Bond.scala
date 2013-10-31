package squantlib.model.bond

import squantlib.util.Date
import squantlib.database.DB
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import squantlib.model.asset.{AnalyzedAsset, Underlying}


case class Bond(
    override val db:dbBond, 
    override val scheduledPayoffs:ScheduledPayoffs,
	override val underlyings:List[String]) 
	extends PriceableBond(db, scheduledPayoffs, underlyings) 
    with YieldAnalysis
    with GreekAnalysis
    with BondAsset {
  
  override val assetStartDate = Some(issueDate)
  
  override val assetEndDate = Some(terminationDate)
  
  override def isPriced: Boolean = dirtyPrice.isDefined
  
  override def latestPriceLocalCcy: Option[Double] = dirtyPrice
  
  override def latestPrice:Option[Double] = dirtyPrice
  
  override def expectedYield:Option[Double] = yieldContinuous
  
  override def expectedCoupon:Option[Double] = currentRate
  
  override def copy:Bond = {
    val bond = Bond(db, scheduledPayoffs, underlyings)
    super.transferSettings(bond)
    bond
  }
  
}

object Bond {
  
  def apply(pb:PriceableBond):Option[Bond] = Some(Bond(pb.db, pb.scheduledPayoffs, pb.underlyings))
  
  def apply(db:dbBond):Option[Bond] = PriceableBond(db).collect{case bond => apply(bond).get}
	
  def apply(db:dbBond, tbdfixing:Option[Double]):Option[Bond] = PriceableBond(db, tbdfixing).collect{case bond => apply(bond).get}
  
  
}

