package net.squantlib.model.asset

import net.squantlib.util.Date
import net.squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import net.squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import net.squantlib.util.initializer.Calendars
import org.jquantlib.time.Calendar


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
  
  def getIntersection(asset:BasicAsset):SortedMap[Date, (Double, Double)] = priceHistory.intersectionWith(asset.priceHistory)

  def historicalCorrelLatestValue(asset:BasicAsset, nbDays:Int, periodicity:Int = 1, minDays:Int = 100):Option[Double] = {
    if (this == asset) {return Some(1.0)}
    val intersection = getIntersection(asset)
    if (intersection.size < minDays) {None}
    val source = intersection takeRight math.min(intersection.size, nbDays)
    
    Correlation.calculate(source, source.size).headOption match {
      case Some(v) if !v._2.isNaN && !v._2.isInfinity => Some(v._2)
      case _ => None
    }
  }
  
  def genericHistoricalCorrel(asset:BasicAsset):Option[Double] = {
    val nbDays = List(30, 60, 180)
    
    if (this == asset) {return Some(1.0)}
    val intersection = getIntersection(asset)
    if (intersection.size < nbDays.min) {None}
    
    val correls = nbDays.filter(_ <= intersection.size).map(n => {
      val (s1, s2) = intersection.values.takeRight(n).toIndexedSeq.unzip
      
      def getcorr(s1:IndexedSeq[Double], s2:IndexedSeq[Double]) = Correlation.calculate(s1, s2).headOption match {
        case Some(v) if !v.isNaN && !v.isInfinity => Some(v)
        case _ => None
      }
      
      List(getcorr(s1, s2), getcorr(s1.drop(1), s2.dropRight(1)), getcorr(s1.dropRight(1), s2.drop(1))).flatMap(s => s) match {
        case vs if vs.isEmpty => None
        case vs => Some(vs.max)
      }
    })
    correls.flatMap(s => s) match {
      case vs if vs.isEmpty => None
      case vs if vs.size == 1 => Some(vs.head)
      case vs => Some(vs.sorted.drop(1).sum / (vs.size - 1.0))
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
        lastmodified = Date.currentTimestamp,
        created = Date.currentTimestamp)
  
  

}
