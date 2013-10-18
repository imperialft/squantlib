package squantlib.model.asset
import org.jquantlib.time.{Weekday, Date => qlDate}
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import java.util.{Date => JavaDate}
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}

/**
 * Simple asset model (no currency)
 */
trait StaticAsset {
  
  var cachedPrice = new WeakHashMap[String, TimeSeries] with SynchronizedMap[String, TimeSeries]
  
  val assetID:String  // to be implemented in subclass
  
  val id:String  // to be implemented in subclass
  
  def latestPrice:Option[Double]  // to be implemented in subclass
  
  def expectedYield:Option[Double]  // to be implemented in subclass
  
  def expectedCoupon:Option[Double]  // to be implemented in subclass
  
  protected def getDbForwardPrice:Map[qlDate, Double]  // to be implemented in subclass
  
  def forwardPrice:TimeSeries = cachedPrice.getOrElseUpdate("FORWARD", TimeSeries(getDbForwardPrice.filter{case (d, _) => isWeekday(d)}))
	
  protected def getPriceHistory:Map[qlDate, Double]
  
  def priceHistory:TimeSeries = cachedPrice.getOrElseUpdate("HISTORICAL", TimeSeries(getPriceHistory.filter{case (d, _) => isWeekday(d)}))
  
  def historicalVolLatest(nbDays:Int, annualDays:Double = 260, minDays:Int = 100):Option[Double] = {
    if (priceHistory.size < minDays) {return None}
    val source = priceHistory takeRight math.min(priceHistory.size, nbDays)
    Volatility.calculate(source, source.size, annualDays).headOption match {
      case Some(v) if !v._2.isNaN && !v._2.isInfinity => Some(v._2)
      case _ => None
    }
  }
  
  def historicalVol(nbDays:Int, annualDays:Double = 260, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
	val source = priceHistory takeRight (if(sourcesize > 0) sourcesize else 10000)
    Volatility.calculate(source, nbDays, annualDays)
  }
  
  def historicalCorrelLatestValue(asset:StaticAsset, nbDays:Int, periodicity:Int = 1, minDays:Int = 100):Option[Double] = {
    if (this == asset) {return Some(1.0)}
    val intersection:SortedMap[qlDate, (Double, Double)] = priceHistory.intersectionWith(asset.priceHistory)
    if (intersection.size < minDays) {None}
    val source = intersection takeRight math.min(intersection.size, nbDays)
    
    Correlation.calculate(source, source.size).headOption match {
      case Some(v) if !v._2.isNaN && !v._2.isInfinity => Some(v._2)
      case _ => None
    }
  }
  
  def historicalCorrelLatest(asset:StaticAsset, nbDays:Int, periodicity:Int = 1, minDays:Int = 100):Option[dbCorrelation] = 
    historicalCorrelLatestValue(asset, nbDays, periodicity, minDays).collect{case v => getCorrelation(asset, StaticAsset.currenttime, nbDays, 1, v)}
  
  def getCorrelation(asset:StaticAsset, valuedate:JavaDate, nbDays:Int, periodicity:Int, correl:Double) = StaticAsset.getCorrelation(this, asset, valuedate, nbDays, periodicity, correl)
  
  def historicalCorrel(asset:StaticAsset, nbDays:Int, nbResult:Int = 0):SortedMap[qlDate, Double] = {
	val sourcesize = nbDays + (if(nbResult > 0) nbResult else 10000) - 1
    val intersection:SortedMap[qlDate, (Double, Double)] = priceHistory.intersectionWith(asset.priceHistory) takeRight sourcesize
    Correlation.calculate(intersection, nbDays)
  }
  
  
  def correlations(asset:StaticAsset, nbDays:Int, nbResult:Int = 0):Set[dbCorrelation] = {
    val correlarray = historicalCorrel(asset, nbDays, nbResult)
    val underlying1 = assetID + ":" + id
    val underlying2 = asset.assetID + ":" + asset.id
    correlarray.map{ case (d, v) => getCorrelation(asset, d.longDate, nbDays, 1, v)} (collection.breakOut)
  }
  
  def isWeekday(d:qlDate):Boolean = d.weekday match {
    case Weekday.Saturday | Weekday.Sunday => false
    case _ => true
  }
    
} 

object StaticAsset {
  
  def getAsset(assetID:String) = assetID match {
    case "FX" => "Currency"
    case "BOND" | "PRICE" => "Bond"
    case a => a
  }
  
  private def currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
  
  def getCorrelation(asset1:StaticAsset, asset2:StaticAsset, valuedate:JavaDate, nbDays:Int, periodicity:Int, correl:Double):dbCorrelation = 
    getCorrelation(asset1.assetID, asset1.id, asset2.assetID, asset2.id, valuedate, nbDays, periodicity, correl)
    
  def getCorrelation(asset1:String, id1:String, asset2:String, id2:String, valuedate:JavaDate, nbDays:Int, periodicity:Int, correl:Double):dbCorrelation = 
    new dbCorrelation(
        id = asset1 + "-" + id1 + ":" + asset2 + "-" + id2,
	    underlying1asset = getAsset(asset1),
	    underlying1id = id1,
	    underlying2asset = getAsset(asset2),
	    underlying2id = id2,
	    valuedate = valuedate,
	    periodicity = periodicity,
	    nbdays = nbDays,
	    value = correl,
	    lastmodified = currenttime)
  
  

}
