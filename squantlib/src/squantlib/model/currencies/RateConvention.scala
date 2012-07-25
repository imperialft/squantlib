package squantlib.model.currencies

import squantlib.parameter.yieldparameter.YieldParameter
import org.jquantlib.indexes.IborIndex
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.{ ActualActual, Thirty360, Actual365Fixed, Actual360, DayCounter }
import org.jquantlib.time.{TimeUnit, Frequency, Date => JDate, Period=>JPeriod}
import scala.collection.immutable.SortedMap

object RateConvention{
	val getConvention = Map(
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

/**
 * Currency specific discount curve calibration.
 */
trait RateConvention {
  import squantlib.parameter.yieldparameter.{SplineNoExtrapolation, FlatVector, LinearNoExtrapolation }
  import squantlib.model.discountcurve.{ CashCurve, SwapCurve, BasisSwapCurve, TenorBasisSwapCurve, SwapPointCurve }
  import org.jquantlib.currencies.America.USDCurrency
  
  	val currency:Currency
  	
	/**
	 * True if rate discounting is applicable. 
	 * No priority against other discounting methods is specified in this class.
	 */
	val useratediscount:Boolean
	
	/**
	 * Returns default short-term cash rate for the given period.
	 */
  	def iborindex(p:JPeriod):IborIndex

  	/**
	 * Defines continuous cash curve constructor.
	 */
	def cash_curve(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2) } 
	
  	/**
	 * Returns cash curve using specified conventions and curve construction method.
	 */
	def cash_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):CashCurve 
		= new CashCurve(cash_curve(valuedate, values), iborindex(new JPeriod(6, TimeUnit.Months)))
  
	def cash_constructor(curve:YieldParameter):CashCurve 
		= new CashCurve(curve, iborindex(new JPeriod(6, TimeUnit.Months)))
	
	/**
	 * Swap floating rate reference.
	 */
	val swap_floatindex:IborIndex
	
	/**
	 * Swap fixed leg day count fraction.
	 */
	val swap_fixdaycount:DayCounter
	
	/**
	 * Swap fixed leg payment frequency.
	 */
	val swap_fixperiod:Frequency
	
  	/**
	 * Defines continuous swap curve constructor.
	 */
	def swap_curve(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)} 
	
  	/**
	 * Returns swap curve using specified conventions and curve construction method.
	 */
	def swap_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):SwapCurve 
		= new SwapCurve(swap_curve(valuedate, values), swap_floatindex, swap_fixdaycount, swap_fixperiod)

	def swap_constructor(curve:YieldParameter):SwapCurve 
		= new SwapCurve(curve, swap_floatindex, swap_fixdaycount, swap_fixperiod)
  
	/**
	 * Floating leg reference for cross currency swap against 3m USD LIBOR.
	 */
	val basis_floatindex:IborIndex = iborindex(new JPeriod(3, TimeUnit.Months))

  	/**
	 * Defines continuous basis swap curve constructor.
	 */
	def basis_curve(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
	
  	/**
	 * Returns basis swap curve using specified conventions and curve construction method.
	 */
	def basis_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):BasisSwapCurve 
		= new BasisSwapCurve(basis_curve(valuedate, values), basis_floatindex)

	def basis_constructor(curve:YieldParameter):BasisSwapCurve 
		= new BasisSwapCurve(curve, basis_floatindex)
  
	/**
	 * Reference rates for tenor basis swap.
	 * Currently only 3 months vs 6 months is supported.
	 */
	val basis36_shortindex:IborIndex = iborindex(new JPeriod(3, TimeUnit.Months))
	val basis36_longindex:IborIndex = iborindex(new JPeriod(6, TimeUnit.Months))
	
  	/**
	 * Defines continuous tenor basis swap curve constructor.
	 */
	def basis36_curve(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
	
  	/**
	 * Returns tenor basis swap curve using specified conventions and curve construction method.
	 */
	def basis36_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):TenorBasisSwapCurve 
		= new TenorBasisSwapCurve(basis_curve(valuedate, values), basis36_shortindex, basis36_longindex)
	
	def basis36_constructor(curve:YieldParameter):TenorBasisSwapCurve 
		= new TenorBasisSwapCurve(curve, basis36_shortindex, basis36_longindex)
  
	/**
	 * True if swap point discounting is applicable
	 */
	val useFXdiscount:Boolean
	
	/**
	 * Number of swap points per 1 unit of FX. Forward fx = spot fx + swap point / swapptmultiplier
	 */
	val swappoint_multiplier:Double
	
	/**
	 * Pivot currency
	 */
	val swappoint_pivot:Currency = new USDCurrency
	
  	/**
	 * Defines continuous swap point curve constructor.
	 */
	def swappoint_curve(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
  
  	/**
	 * Returns tenor basis swap curve using specified conventions and curve construction method.
	 */
	def swappoint_constructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):SwapPointCurve 
		= new SwapPointCurve(swappoint_curve(valuedate, values), swappoint_multiplier, currency, swappoint_pivot)
  
	def swappoint_constructor(curve:YieldParameter):SwapPointCurve 
		= new SwapPointCurve(curve, swappoint_multiplier, currency, swappoint_pivot)
}


class JpyRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.JPYLibor
  import org.jquantlib.currencies.Asia.JPYCurrency
  
  	val currency = new JPYCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new JPYLibor(p)
	val swap_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
	
	val useFXdiscount = false
	val swappoint_multiplier = 100.0
}

class UsdRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.USDLibor
  import org.jquantlib.currencies.America.USDCurrency
  
  	val currency = new USDCurrency
  	
	val useratediscount = true
	def iborindex(p:JPeriod) = new USDLibor(p)
	val swap_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual360
	val swap_fixperiod = Frequency.Annual
	
	val useFXdiscount = false
	val swappoint_multiplier = -99999.0
}

class EurRateConvention extends RateConvention{
  import org.jquantlib.indexes.Euribor
  import org.jquantlib.currencies.Europe.EURCurrency
  
  	val currency = new EURCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new Euribor(p)
	val swap_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class AudRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.BBSW
  import org.jquantlib.currencies.Oceania.AUDCurrency
  
  	val currency = new AUDCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new BBSW(p)
	val swap_floatindex = new BBSW(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class CadRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Cdor
  import org.jquantlib.currencies.America.CADCurrency
  
  	val currency = new CADCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new Cdor(p)
	val swap_floatindex = new Cdor(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class GbpRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.GBPLibor
  import org.jquantlib.currencies.Europe.GBPCurrency
  
  	val currency = new GBPCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new GBPLibor(p)
	val swap_floatindex = new GBPLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class ZarRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Jibar
  import org.jquantlib.currencies.Africa.ZARCurrency
  
  	val currency = new ZARCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new Jibar(p)
	val swap_floatindex = new Jibar(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Quarterly

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class HufRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.HUFLibor
  import org.jquantlib.currencies.Europe.HUFCurrency
  
  	val currency = new HUFCurrency
  
	val useratediscount = false
	def iborindex(p:JPeriod) = new HUFLibor(p)
	val swap_floatindex = new HUFLibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 100.0
}

class IdrRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.IDRCurrency
  
  	val currency = new IDRCurrency
  	
	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 1.0
}

class BrlRateConvention extends RateConvention{
  import org.jquantlib.currencies.America.BRLCurrency
  
  	val currency = new BRLCurrency
  	
	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class CnyRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.CNYCurrency
  
  	val currency = new CNYCurrency

  	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class InrRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.INRCurrency
  
  	val currency = new INRCurrency

  	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 100.0
}

class KrwRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.KRWCurrency
  
  	val currency = new KRWCurrency

  	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 1.0
}

class MxnRateConvention extends RateConvention{
  import org.jquantlib.currencies.America.MXNCurrency
  
  	val currency = new MXNCurrency
  
	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class NzdRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.BKBM
  import org.jquantlib.currencies.Oceania.NZDCurrency
  
  	val currency = new NZDCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new BKBM(p)
	val swap_floatindex = new BKBM(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

class PlnRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Wibor
  import org.jquantlib.currencies.Europe.PLNCurrency
  
  	val currency = new PLNCurrency
  
	val useratediscount = false
	def iborindex(p:JPeriod) = new Wibor(p)
	val swap_floatindex = new Wibor(new JPeriod(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class RonRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.RONCurrency
  
  	val currency = new RONCurrency

  	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class RubRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.RUBCurrency
  
  	val currency = new RUBCurrency

  	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class SekRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.STIBOR
  import org.jquantlib.currencies.Europe.SEKCurrency
  
  	val currency = new SEKCurrency
  
	val useratediscount = true
	def iborindex(p:JPeriod) = new STIBOR(p)
	val swap_floatindex = new STIBOR(new JPeriod(3, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

class TryRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.TRYCurrency
  
  	val currency = new TRYCurrency
  	
	val useratediscount = false
	def iborindex(p:JPeriod) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}

