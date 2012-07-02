package squantlib.model.currencies

import squantlib.parameter.yieldparameter.YieldParameter
import org.jquantlib.indexes.IborIndex
import org.jquantlib.daycounters.{ ActualActual, Thirty360, Actual365Fixed, Actual360, DayCounter }
import org.jquantlib.time.{TimeUnit, Frequency, Date => JDate, Period=>JPeriod}
import scala.collection.immutable.SortedMap

/**
 * Currency specific settings
 *
 */
trait RateCurrencySetting {
  import squantlib.parameter.yieldparameter.SplineNoExtrapolation
  
	def iborindex(p:JPeriod):IborIndex
	def cash_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= new SplineNoExtrapolation(valuedate, values, 2)
	
	val swap_floatindex:IborIndex
	val swap_fixdaycount:DayCounter
	val swap_fixperiod:Frequency
	def swap_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= new SplineNoExtrapolation(valuedate, values, 2)
  
	val basis_floatindex:IborIndex = iborindex(new JPeriod(3, TimeUnit.Months))
	def basis_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= new SplineNoExtrapolation(valuedate, values, 2)
  
	val basis36_sindex:IborIndex = iborindex(new JPeriod(3, TimeUnit.Months))
	val basis36_lindex:IborIndex = iborindex(new JPeriod(6, TimeUnit.Months))
	def basis36_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter	
		= new SplineNoExtrapolation(valuedate, values, 2)
}


class JpyCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.JPYLibor
  
	def iborindex(p:JPeriod) = new JPYLibor(p)
	val swap_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
}

class UsdCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.USDLibor
  
	def iborindex(p:JPeriod) = new USDLibor(p)
	val swap_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual360
	val swap_fixperiod = Frequency.Annual
}

class EurCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.Euribor
  
	def iborindex(p:JPeriod) = new Euribor(p)
	val swap_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual
}

class AudCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.BBSW
  
	def iborindex(p:JPeriod) = new BBSW(p)
	val swap_floatindex = new BBSW(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
}

class CadCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.Cdor
  
	def iborindex(p:JPeriod) = new Cdor(p)
	val swap_floatindex = new Cdor(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
}

class GbpCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.GBPLibor
  
	def iborindex(p:JPeriod) = new GBPLibor(p)
	val swap_floatindex = new GBPLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
}

class ZarCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.Jibar
  
	def iborindex(p:JPeriod) = new Jibar(p)
	val swap_floatindex = new Jibar(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Quarterly
}

class HufCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.HUFLibor
  
	def iborindex(p:JPeriod) = new HUFLibor(p)
	val swap_floatindex = new HUFLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Annual
}

class IdrCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class BrlCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class CnyCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class InrCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class KrwCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class MxnCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class NzdCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.BKBM
  
	def iborindex(p:JPeriod) = new BKBM(p)
	val swap_floatindex = new BKBM(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
}

class PlnCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.Wibor
  
	def iborindex(p:JPeriod) = new Wibor(p)
	val swap_floatindex = new Wibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual
}

class RonCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class RubCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null
}

class SekCurrencySetting extends RateCurrencySetting{
  import org.jquantlib.indexes.ibor.STIBOR
  
	def iborindex(p:JPeriod) = new STIBOR(p)
	val swap_floatindex = new STIBOR(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual
}

class TryCurrencySetting extends RateCurrencySetting{
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

}

