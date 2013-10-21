package squantlib.model.asset

import squantlib.util.Date
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.time.Calendar

case class GenericAsset(
  override val assetID:String,
  override val id:String,
  override val latestPrice:Option[Double],
  override val expectedYield:Option[Double],
  override val expectedCoupon:Option[Double],
  override val getDbForwardPrice:Map[Date, Double],
  override val getPriceHistory:Map[Date, Double],
  override val calendar:Calendar
  ) extends StaticAsset
  
