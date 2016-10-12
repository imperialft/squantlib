package net.squantlib.model.asset

import net.squantlib.util.Date
import net.squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import net.squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import net.squantlib.util.initializer.Calendars
import org.jquantlib.time.Calendar
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
  
  def fixingCalendar:Calendar // to be implemented in subclass

  def paymentCalendar:Calendar // to be implemented in subclass

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
  
  def priceHistory:TimeSeries = cachedPrice.getOrElseUpdate("HISTORICAL", getPriceHistory.getBusinessDayFilledTimeSeries(fixingCalendar))
  
  def priceHistory(cal:Calendar):TimeSeries = priceHistory.filter{case (d, v) => d.isBusinessday(cal)}
  
  def getLivePriceHistory:TimeSeries = 
    if (priceHistory.isEmpty) TimeSeries.empty
    else TimeSeries(priceHistory.filterKeys(d => (d ge priceStartDate) && (d le priceEndDate)))
  
  lazy val firstData = priceHistory.firstDate

  lazy val lastData = priceHistory.lastDate
  
  lazy val priceStartDate:Date = assetStartDate.collect{case d => if (d ge firstData) d else firstData}.getOrElse(firstData)
  
  lazy val priceEndDate:Date = assetEndDate.collect{case d => if (d le lastData) d else lastData}.getOrElse(lastData)
  
  def isAliveOn(d:Date):Boolean = (d ge priceStartDate) && (d le priceEndDate)
  
  /*
   * Forward Price
   */
  
  protected def getDbForwardPrice:TimeSeries  // to be implemented in subclass
  
  def forwardPrice:TimeSeries = cachedPrice.getOrElseUpdate("FORWARD", getDbForwardPrice.getFilledTimeSeries())
  
  
  var cachedPrice = new WeakHashMap[String, TimeSeries] with SynchronizedMap[String, TimeSeries]
  
} 