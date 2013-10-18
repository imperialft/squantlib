package squantlib.model.asset
import org.jquantlib.time.{Weekday, Date => qlDate}
import squantlib.math.timeseries.{TimeSeries, Correlation, Volatility}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import scala.collection.SortedMap
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import java.util.{Date => JavaDate}
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.schemadefinitions.{Correlation => dbCorrelation}
import org.jquantlib.currencies.Currency

/**
 * StaticAsset with currency & analysis features
 */
trait AnalyzedAsset extends StaticAsset {
  
  val currency:Currency
  
  def latestPriceLocalCcy:Option[Double]  // to be implemented in subclass
  
  def isPriced:Boolean
  
} 

