package squantlib.model.asset

import squantlib.util.Date
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import squantlib.util.initializer.Calendars
import org.jquantlib.time.Calendar
import org.jquantlib.time.calendars.NullCalendar
import org.jquantlib.currencies.Currency

trait StaticAsset extends BasicAsset with StaticAnalysis 

/**
 * Simple asset model
 */
trait BasicAsset {
  
  /*
   * Basic information
   */
  
  val assetID:String  // to be implemented in subclass "Bond" "Currency" etc
  
  val assetName:String  // to be implemented in subclass "USDJPY", "ADB-00001", etc
  
  def currency:Currency // to be implemented in subclass
  
  def isPriced:Boolean  // to be implemented in subclass
  
  def assetStartDate:Option[Date] = None // to be implemented in subclass
  
  def assetEndDate:Option[Date] = None // to be implemented in subclass
  
  def calendar:Calendar // to be implemented in subclass
  
  /*
   * Spot price
   */
  
  def latestPrice:Option[Double]  // to be implemented in subclass
  
  def latestPriceDate:Option[Date] // to be implemented in subclass
  
  def latestPriceLocalCcy:Option[Double]  // to be implemented in subclass
  
  def expectedYield:Option[Double]  // to be implemented in subclass
  
  def expectedCoupon:Option[Double]  // to be implemented in subclass
  
  
  /*
   * Historical Price
   */
  
  protected def getPriceHistory:TimeSeries // to be implemented in subclass
  
  def priceHistory:TimeSeries = cachedPrice.getOrElseUpdate("HISTORICAL", getPriceHistory.getBusinessDayFilledTimeSeries(calendar))
  
  def priceHistory(cal:Calendar):TimeSeries = priceHistory.filter{case (d, v) => d.isBusinessday(cal)}
  
  def getLivePriceHistory:TimeSeries = TimeSeries(priceHistory.filterKeys(d => 
    priceStartDate.collect{case start => d ge start}.getOrElse(true) && priceEndDate.collect{case end => d le end}.getOrElse(true)))
  
  lazy val firstData:Option[Date] = priceHistory.firstDate

  lazy val lastData:Option[Date] = priceHistory.lastDate
  
  lazy val priceStartDate:Option[Date] = (assetStartDate, firstData) match {
    case (Some(d), Some(firstdata)) if d ge firstdata => assetStartDate
    case _ => firstData
    }
  
  lazy val priceEndDate:Option[Date] = (assetEndDate, lastData) match {
    case (Some(d), Some(lastdata)) if d le lastdata => assetEndDate
    case _ => lastData
  }
  
  def isAliveOn(d:Date):Boolean = (priceStartDate, priceEndDate) match {
    case (Some(start), Some(end)) => (d ge start) && (d le end)
    case _ => false
  } 
  
  /*
   * Forward Price
   */
  
  protected def getDbForwardPrice:TimeSeries  // to be implemented in subclass
  
  def forwardPrice:TimeSeries = cachedPrice.getOrElseUpdate("FORWARD", getDbForwardPrice.getFilledTimeSeries())
  
  
  var cachedPrice = new WeakHashMap[String, TimeSeries] with SynchronizedMap[String, TimeSeries]
  
} 

