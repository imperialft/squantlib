package net.squantlib.model.rates.convention

import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Period => qlPeriod, TimeUnit, Frequency}

/**
 * Currency specific discount curve calibration.
 */
trait RateConvention {
  
  	val currency:Currency
  	
	/**
	 * True if rate discounting is applicable. 
	 * No priority against other discounting methods is specified in this class.
	 */
	val useRateDiscount:Boolean
	
	/**
	 * Returns default short-term cash rate for the given period.
	 */
  	def iborindex(p:qlPeriod):IborIndex

	/**
	 * Swap floating rate reference.
	 */
	val swapFloatIndex:IborIndex
	
	/**
	 * Swap fixed leg day count fraction.
	 */
	val swapFixDaycount:DayCounter
	
	/**
	 * Swap fixed leg payment frequency.
	 */
	val swapFixPeriod:Frequency
	
  
	/**
	 * Floating leg reference for cross currency swap against 3m USD LIBOR.
	 */
	val basisFloatIndex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))

	/**
	 * Reference rates for tenor basis swap.
	 * Currently only 3 months vs 6 months is supported.
	 */
	val basis36ShortIndex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))
	val basis36LongIndex:IborIndex = iborindex(new qlPeriod(6, TimeUnit.Months))
	
  
	/**
	 * True if swap point discounting is applicable
	 */
	val useFXdiscount:Boolean
	
	/**
	 * Number of swap points per 1 unit of FX. Forward fx = spot fx + swap point / swapptmultiplier
	 */
	val swapPointMultiplier:Double
	
	/**
	 * Pivot currency
	 */
	val swapPointPivotCcy:Currency = new USDCurrency
	
	
	/**
	 * True if NDS discounting is applicable
	 */
	val useNDSdiscount:Boolean = false
	
	/**
	 * CDS fixed leg daycount, paid in local currency
	 */
	val ndsFixDaycount:DayCounter = null
	
	/**
	 * CDS fixed leg payment frequency, paid in local currency
	 */
	val ndsFixPeriod:Frequency = Frequency.Semiannual
	
	/**
	 * CDS float leg payment index, paid in pivot currency (usually USD)
	 */
	val ndsFloatIndex:IborIndex = new USDLibor(new qlPeriod("6M"))
	
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


