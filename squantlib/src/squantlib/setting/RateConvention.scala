package squantlib.setting

import squantlib.setting.rateconventions._
import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.indexes.IborIndex
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.{ ActualActual, Thirty360, Actual365Fixed, Actual360, DayCounter }
import org.jquantlib.time.{TimeUnit, Frequency, Date => qlDate, Period=>qlPeriod}
import scala.collection.SortedMap
import squantlib.model.yieldparameter.{SplineNoExtrapolation, FlatVector, LinearNoExtrapolation }
import squantlib.model.rates.{ CashCurve, SwapCurve, BasisSwapCurve, TenorBasisSwapCurve, SwapPointCurve }
import org.jquantlib.currencies.America.USDCurrency


/**
 * Currency specific discount curve calibration.
 */
trait RateConvention {
  
  	val currency:Currency
  	
	/**
	 * True if rate discounting is applicable. 
	 * No priority against other discounting methods is specified in this class.
	 */
	val useratediscount:Boolean
	
	/**
	 * Returns default short-term cash rate for the given period.
	 */
  	def iborindex(p:qlPeriod):IborIndex

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
	 * Floating leg reference for cross currency swap against 3m USD LIBOR.
	 */
	val basis_floatindex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))

	/**
	 * Reference rates for tenor basis swap.
	 * Currently only 3 months vs 6 months is supported.
	 */
	val basis36_shortindex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))
	val basis36_longindex:IborIndex = iborindex(new qlPeriod(6, TimeUnit.Months))
	
  
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
	def swappoint_curve(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
  
  	/**
	 * Returns tenor basis swap curve using specified conventions and curve construction method.
	 */
	def swappoint_constructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):SwapPointCurve 
		= new SwapPointCurve(swappoint_curve(valuedate, values), swappoint_multiplier, currency, swappoint_pivot)
  
	def swappoint_constructor(curve:YieldParameter):SwapPointCurve 
		= new SwapPointCurve(curve, swappoint_multiplier, currency, swappoint_pivot)
}


object RateConvention {

  	def apply(id:String):Option[RateConvention] = mapper.get(id)
  	
  	def toMap:Map[String, RateConvention] = mapper
  
  	def contains(id:String):Boolean = mapper.contains(id)
  	
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


