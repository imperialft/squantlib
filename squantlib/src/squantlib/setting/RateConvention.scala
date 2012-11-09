package squantlib.setting

import squantlib.parameter.yieldparameter.YieldParameter
import org.jquantlib.indexes.IborIndex
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.{ ActualActual, Thirty360, Actual365Fixed, Actual360, DayCounter }
import org.jquantlib.time.{TimeUnit, Frequency, Date => qlDate, Period=>qlPeriod}
import scala.collection.SortedMap
import squantlib.parameter.yieldparameter.{SplineNoExtrapolation, FlatVector, LinearNoExtrapolation }
import squantlib.model.discountcurve.{ CashCurve, SwapCurve, BasisSwapCurve, TenorBasisSwapCurve, SwapPointCurve }
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
	 * Defines continuous cash curve constructor.
	 */
	def cash_curve(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2) } 
	
  	/**
	 * Returns cash curve using specified conventions and curve construction method.
	 */
	def cash_constructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):CashCurve 
		= new CashCurve(cash_curve(valuedate, values), iborindex(new qlPeriod(6, TimeUnit.Months)))
  
	def cash_constructor(curve:YieldParameter):CashCurve 
		= new CashCurve(curve, iborindex(new qlPeriod(6, TimeUnit.Months)))
	
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
	def swap_curve(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)} 
	
  	/**
	 * Returns swap curve using specified conventions and curve construction method.
	 */
	def swap_constructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):SwapCurve 
		= new SwapCurve(swap_curve(valuedate, values), swap_floatindex, swap_fixdaycount, swap_fixperiod)

	def swap_constructor(curve:YieldParameter):SwapCurve 
		= new SwapCurve(curve, swap_floatindex, swap_fixdaycount, swap_fixperiod)
  
	/**
	 * Floating leg reference for cross currency swap against 3m USD LIBOR.
	 */
	val basis_floatindex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))

  	/**
	 * Defines continuous basis swap curve constructor.
	 */
	def basis_curve(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
	
  	/**
	 * Returns basis swap curve using specified conventions and curve construction method.
	 */
	def basis_constructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):BasisSwapCurve 
		= new BasisSwapCurve(basis_curve(valuedate, values), basis_floatindex)

	def basis_constructor(curve:YieldParameter):BasisSwapCurve 
		= new BasisSwapCurve(curve, basis_floatindex)
  
	/**
	 * Reference rates for tenor basis swap.
	 * Currently only 3 months vs 6 months is supported.
	 */
	val basis36_shortindex:IborIndex = iborindex(new qlPeriod(3, TimeUnit.Months))
	val basis36_longindex:IborIndex = iborindex(new qlPeriod(6, TimeUnit.Months))
	
  	/**
	 * Defines continuous tenor basis swap curve constructor.
	 */
	def basis36_curve(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case 2 => new LinearNoExtrapolation(valuedate, values)
			case _ => new SplineNoExtrapolation(valuedate, values, 2)
  		} 
	
  	/**
	 * Returns tenor basis swap curve using specified conventions and curve construction method.
	 */
	def basis36_constructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):TenorBasisSwapCurve 
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


