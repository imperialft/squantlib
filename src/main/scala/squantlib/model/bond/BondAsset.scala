package squantlib.model.bond

import scala.collection.LinearSeq
import scala.collection.mutable.{Map => MutableMap, WeakHashMap, SynchronizedMap}
import squantlib.model.asset.AnalyzedAsset
import squantlib.util.DisplayUtils._
import squantlib.math.timeseries.TimeSeries
import squantlib.model.market.Market
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.util.Date
import squantlib.database.DB
import org.jquantlib.currencies.Currency

trait BondAsset extends AnalyzedAsset {
  
  val db:dbBond
  
  override val assetStartDate:Option[Date] = Some(db.issueDate)
  
  override val assetEndDate:Option[Date] = Some(db.endDate)
  
  override def isPriced:Boolean
  
  override def latestPriceLocalCcy: Option[Double]
  
  override val assetID = "PRICE"
    
  override val assetName = db.id
    
  override def getPriceHistory = DB.getHistorical("BONDJPY:" + assetName)
  
  override def latestPrice:Option[Double]
  
  override def expectedYield:Option[Double]
  
  override def expectedCoupon:Option[Double]
  
  override def getDbForwardPrice = DB.getForwardPrices("BOND", assetName)
  
} 

