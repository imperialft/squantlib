package squantlib.setting.initializer

import squantlib.setting.rateconventions._
import squantlib.setting.RateConvention

object RateConventions extends Initializer[RateConvention] {
	val mapper = Map(
			("AUD" -> new AudRateConvention),
			("BRL" -> new BrlRateConvention),
			("CAD" -> new CadRateConvention), 
			("CNY" -> new CnyRateConvention),
			("EUR" -> new EurRateConvention),
			("GBP" -> new GbpRateConvention),
			("HUF" -> new HufRateConvention),
			("IDR" -> new IdrRateConvention),
			("INR" -> new InrRateConvention),
			("JPY" -> new JpyRateConvention),
			("KRW" -> new KrwRateConvention),
			("MXN" -> new MxnRateConvention),
			("NZD" -> new NzdRateConvention),
			("PLN" -> new PlnRateConvention),
			("RON" -> new RonRateConvention),
			("RUB" -> new RubRateConvention),
			("SEK" -> new SekRateConvention),
			("TRY" -> new TryRateConvention),
			("USD" -> new UsdRateConvention),
			("ZAR" -> new ZarRateConvention))
			
}

