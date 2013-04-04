package squantlib.model

import org.jquantlib.time.{Weekday, Date => qlDate, Period => qlPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Volatility => dbVolatility, Correlation => dbCorrelation}
import scala.collection.SortedMap


/**
 * Underlying to be used for pricing models.
 */
trait StaticAsset {
  
  var cachedPrice = new scala.collection.mutable.WeakHashMap[String, TimeSeries] 
  
  val assetID:String
  
  val id:String
  
  def latestPrice:Option[Double]
  
  def expectedYield:Option[Double]
  
  def expectedCoupon:Option[Double]
	
  protected def getHistoricalPrice:Map[qlDate, Double]
  
  def historicalPrice:TimeSeries = cachedPrice.getOrElseUpdate("HISTORICAL", TimeSeries(getHistoricalPrice.filter{case (d, _) => isWeekday(d)}))
  
  def historicalVolLatest(nbDays:Int, annualDays:Double = 260):Option[Double] = historicalVol(nbDays, annualDays, 1).headOption.collect{case s => s._2}
  
  def historicalVol(nbDays:Int, annualDays:Double = 260, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
	val source = historicalPrice takeRight (if(sourcesize > 0) sourcesize else 10000)
    Volatility.calculate(source, nbDays, annualDays)
  }
  
  def volatilities(nbDays:Int, annualDays:Int = 260, nbResult:Int = 0):Set[dbVolatility] = {
    val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
    val volarray = historicalVol(nbDays, annualDays, nbResult)
    volarray.map { case (d, v) =>
      new dbVolatility(
          id = (assetID + ":" + id + ":" + ("%tY%<tm%<td" format d.longDate) + ":" + 1 + ":" + nbDays),
	      underlying = id,
	      valuedate = d.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v,
	      lastmodified = Some(currenttime))
    	} (collection.breakOut)
  }
  
  def historicalCorrelLatest(asset:StaticAsset, nbDays:Int):Option[Double] = {
    if (this == asset) Some(1.0)
    else historicalCorrel(asset, nbDays, 1).headOption.collect{case s => s._2}
  }
  
  def historicalCorrel(asset:StaticAsset, nbDays:Int, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
    val intersection:SortedMap[qlDate, (Double, Double)] = historicalPrice.intersectionWith(asset.historicalPrice) takeRight sourcesize
    Correlation.calculate(intersection, nbDays)
  }
  
  def correlations(asset:StaticAsset, nbDays:Int, nbResult:Int = 0):Set[dbCorrelation] = {
    val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
    val correlarray = historicalCorrel(asset, nbDays, nbResult)
    val underlying1 = assetID + ":" + id
    val underlying2 = asset.assetID + ":" + asset.id
    correlarray.map { case (d, v) =>
      new dbCorrelation(
          id = (underlying1 + ":" + underlying2 + ":" + ("%tY%<tm%<td" format d.longDate) + ":" + 1 + ":" + nbDays),
	      underlying1 = underlying1,
	      underlying2 = underlying2,
	      valuedate = d.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v,
	      lastmodified = Some(currenttime))
   	} (collection.breakOut)
  }
  
  def isWeekday(d:qlDate):Boolean = d.weekday match {
    case Weekday.Saturday | Weekday.Sunday => false
    case _ => true
  }

    
} 