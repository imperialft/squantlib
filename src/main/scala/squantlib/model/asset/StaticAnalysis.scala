package squantlib.model.asset

import squantlib.util.Date
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import squantlib.util.initializer.Calendars
import org.jquantlib.time.Calendar
import org.jquantlib.time.calendars.NullCalendar


trait StaticAnalysis {
  self : BasicAsset =>
    
  def historicalVolLatest(nbDays:Int, annualDays:Double = 260, minDays:Int = 100):Option[Double] = {
    if (priceHistory.size < minDays) {return None}
    val source = priceHistory takeRight math.min(priceHistory.size, nbDays)
    Volatility.calculate(source, source.size, annualDays).headOption match {
      case Some(v) if !v._2.isNaN && !v._2.isInfinity => Some(v._2)
      case _ => None
    }
  }
  
  def historicalVol(nbDays:Int, annualDays:Double = 260, nbResult:Int = 0):SortedMap[Date, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
	val source = priceHistory takeRight (if(sourcesize > 0) sourcesize else 10000)
    Volatility.calculate(source, nbDays, annualDays)
  }
  
  def historicalCorrelLatestValue(asset:BasicAsset, nbDays:Int, periodicity:Int = 1, minDays:Int = 100):Option[Double] = {
    if (this == asset) {return Some(1.0)}
    val intersection:SortedMap[Date, (Double, Double)] = priceHistory.intersectionWith(asset.priceHistory)
    if (intersection.size < minDays) {None}
    val source = intersection takeRight math.min(intersection.size, nbDays)
    
    Correlation.calculate(source, source.size).headOption match {
      case Some(v) if !v._2.isNaN && !v._2.isInfinity => Some(v._2)
      case _ => None
    }
  }
  
  def historicalCorrelLatest(asset:BasicAsset, nbDays:Int, periodicity:Int = 1, minDays:Int = 100):Option[dbCorrelation] = 
    historicalCorrelLatestValue(asset, nbDays, periodicity, minDays).collect{case v => getCorrelation(asset, Date.currentDate, nbDays, 1, v)}
  
  def getCorrelation(asset:BasicAsset, valuedate:Date, nbDays:Int, periodicity:Int, correl:Double) = StaticAnalysis.getCorrelation(this, asset, valuedate, nbDays, periodicity, correl)
  
  def historicalCorrel(asset:BasicAsset, nbDays:Int, nbResult:Int = 0):SortedMap[Date, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
    val intersection:SortedMap[Date, (Double, Double)] = priceHistory.intersectionWith(asset.priceHistory) takeRight sourcesize
    Correlation.calculate(intersection, nbDays)
  }
  
  
  def correlations(asset:BasicAsset, nbDays:Int, nbResult:Int = 0):Set[dbCorrelation] = {
    val correlarray = historicalCorrel(asset, nbDays, nbResult)
    val underlying1 = assetID + ":" + assetName
    val underlying2 = asset.assetID + ":" + asset.assetName
    correlarray.map{ case (d, v) => getCorrelation(asset, d, nbDays, 1, v)} (collection.breakOut)
  }    
}

object StaticAnalysis {
  
  def getAsset(assetID:String) = assetID match {
    case "FX" => "Currency"
    case "BOND" | "PRICE" => "Bond"
    case a => a
  }
  
  def getCorrelation(asset1:BasicAsset, asset2:BasicAsset, valuedate:Date, nbDays:Int, periodicity:Int, correl:Double):dbCorrelation = 
    getCorrelation(asset1.assetID, asset1.assetName, asset2.assetID, asset2.assetName, valuedate, nbDays, periodicity, correl)
    
  def getCorrelation(asset1:String, id1:String, asset2:String, id2:String, valuedate:Date, nbDays:Int, periodicity:Int, correl:Double):dbCorrelation = 
    new dbCorrelation(
        id = asset1 + "-" + id1 + ":" + asset2 + "-" + id2,
	    underlying1asset = getAsset(asset1),
	    underlying1id = id1,
	    underlying2asset = getAsset(asset2),
	    underlying2id = id2,
	    valuedate = valuedate.java,
	    periodicity = periodicity,
	    nbdays = nbDays,
	    value = correl,
	    lastmodified = Date.currentTimestamp)
  
  

}
