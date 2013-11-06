package squantlib.model.asset

import squantlib.util.Date
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.time.Calendar
import squantlib.math.timeseries.TimeSeries
import squantlib.util.initializer.Currencies

case class GenericAsset(
  override val assetID:String,
  override val assetName:String,
  override val latestPrice:Option[Double],
  override val expectedYield:Option[Double],
  override val expectedCoupon:Option[Double],
  override val getDbForwardPrice:TimeSeries,
  override val getPriceHistory:TimeSeries,
  override val calendar:Calendar,
  override val currency:Currency = Currencies("JPY").get,
  override val isPriced:Boolean = true
  ) extends StaticAsset {
  
  override def latestPriceDate = None
  
  override def latestPriceLocalCcy = latestPrice
  
}
  
