package net.squantlib.model.asset

import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.time.Calendar
import net.squantlib.math.timeseries.TimeSeries
import net.squantlib.util.initializer.Currencies
import net.squantlib.util.{FixingInformation, Date, DbCalendar}

case class GenericAsset(
  override val assetID:String,
  override val assetName:String,
  override val latestPrice:Option[Double],
  override val expectedYield:Option[Double],
  override val expectedCoupon:Option[Double],
  override val getDbForwardPrice:TimeSeries,
  override val getPriceHistory:TimeSeries,
  override val fixingCalendar:DbCalendar,
  override val paymentCalendar:DbCalendar,
  override val currency:Currency = Currencies("JPY").get,
  override val isPriced:Boolean = true
  ) extends StaticAsset {
  
  override def latestPriceDate = None
  
  override def latestPriceLocalCcy = latestPrice
  
}
  
