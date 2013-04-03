package squantlib.model

import org.jquantlib.time.{Weekday, Date => qlDate, Period => qlPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Volatility => dbVolatility, Correlation => dbCorrelation}
import scala.collection.SortedMap


/**
 * Underlying to be used for pricing models.
 */
trait Asset {
  
  var cachedPrice = new scala.collection.mutable.WeakHashMap[String, TimeSeries] 
  
  val assetID:String
  
  val id:String
  
  def spot:Double  
	
  protected def getHistoricalPrice:Map[qlDate, Double]
  
  def historicalPrice:TimeSeries = cachedPrice.getOrElseUpdate("HISTORICAL", TimeSeries(getHistoricalPrice.filter{case (d, _) => isWeekday(d)}))
	  
  def historicalVolatility(nbDays:Int, annualDays:Double = 260, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000)
	val source = historicalPrice takeRight (if(sourcesize > 0) sourcesize else 10000)
    Volatility.calculate(source, nbDays, annualDays)
  }
  
  def volatilities(nbDays:Int, annualDays:Int = 260, nbResult:Int = 0):Set[dbVolatility] = {
    val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
    val volarray = historicalVolatility(nbDays, annualDays, nbResult)
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
    
  def historicalCorrelation(asset:Asset, nbDays:Int, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
    val intersection = historicalPrice.intersectionWith(asset.historicalPrice) takeRight sourcesize
    Correlation.calculate(intersection, nbDays)
  }
  
  def correlations(asset:Asset, nbDays:Int, nbResult:Int = 0):Set[dbCorrelation] = {
    val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
    val correlarray = historicalCorrelation(asset, nbDays, nbResult)
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