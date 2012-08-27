package squantlib.model.discountcurve

import scala.collection.immutable.{TreeMap, SortedSet}
import squantlib.parameter.yieldparameter.{YieldParameter, SplineEExtrapolation, SplineNoExtrapolation, LinearNoExtrapolation}
import org.jquantlib.time.{ Date => JDate, Period => JPeriod, TimeUnit}
import org.jquantlib.daycounters.DayCounter;
import squantlib.database.schemadefinitions.RateFXParameter
//import squantlib.model.discountcurve.LiborDiscountCurve
import squantlib.initializer.RateConvention
//import org.jquantlib.time.{Period => JPeriod, Date => JDate}
//import org.jquantlib.currencies.Currency



 
  /**
   * Libor discounting model
   * - 3m/6m basis is paid semiannually instead of quarterly (small error)
   * - zero rate volatility (model assumption)
   * - no 3m-Xm basis for X < 6 (implied by ZC interpolation 3m & 6m)
   * - no 6m-Xm basis for X > 6 (implied by ZC interpolation 6m & 12m)
   */
class LiborDiscountCurve (val cash:CashCurve, val swap:SwapCurve, val basis:BasisSwapCurve, val tenorbasis:TenorBasisSwapCurve, val fx : Double) 
extends RateCurve{
  require (
		(cash == null || (cash.valuedate == swap.valuedate && cash.currency == swap.currency && cash.floatindex.dayCounter == swap.floatindex.dayCounter))
		&& (swap == null || swap.valuedate == swap.valuedate)
		&& (basis == null || (basis.valuedate == swap.valuedate && basis.currency == swap.currency))
		&& (tenorbasis == null || (tenorbasis.valuedate == swap.valuedate && tenorbasis.currency == swap.currency)))

	  val currency = cash.currency
	  val valuedate = swap.valuedate
	  
	  /**
	   * swap specifications
	   * we use yearly day count factor instead of exact calendar dates as estimate in few cases, with small potential error
	   */
	  val floattenor = swap.floatindex.tenor().length()	  
	  val fixperiod = 12 / swap.fixperiod.toInteger()
  	  val fixfraction = swap.fixdaycount.annualDayCount()
	  val floatfraction = swap.floatindex.dayCounter().annualDayCount()
	  
	  /**
	   * day count initialization, for swap fixed leg convention. (not to be used for cash rate)
	   */
	  val maxmaturity = JPeriod.months(swap.rate.maxperiod, valuedate).toInt
	  val zcmonths:Seq[Int] = (for (m <- (List(0, 3, 6, 9) ++ (12 to maxmaturity by fixperiod))) yield m).sorted
	  val zcperiods = TreeMap(zcmonths.map(m => (m, new JPeriod(m, TimeUnit.Months))) : _*) 
	  val maturities = TreeMap(zcmonths.map(m => (m, valuedate.add(zcperiods(m)))) : _*) 
	  val fixdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
			  		.map(m => (m, swap.fixdaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
	  val floatdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  				.map(m => (m, swap.floatindex.dayCounter().yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
	  				
	  /**
	   * using cash rate to compute zero coupon < 12 months.
	   */
	  val swapstart = 12;				
	  val cashrange = zcperiods.filter(m => (m._1 < swapstart && m._1 > 0))
	  val swaprange = zcperiods.filter(m => m._1 >= swapstart)

	  /**
	   * 3m/6m basis swap calibration is valid in case float leg is semi annual (ccy basis always quarterly)
	   */
	  val bs3m6madjust = if (tenorbasis == null && floattenor > 3) null 
	  					 else zcperiods.map(m => (m._1, m._1 match { 
	  					    case n if n < swapstart && n < 6 => 0.0
						    case n if n < swapstart && n >= 6 => if (tenorbasis == null) 0.0 else tenorbasis.value(m._2)
						    case n if n >= swapstart && floattenor <= 3 => 0.0
						    case _ => tenorbasis.value(m._2) }))
  
	  
	  /**
	   * true if this currency is the "pivot" currency for the basis swap, usually USD.
	   * no support for additional pivot currency.
	   */
	  val ispivotcurrency = swap.currency == BasisSwapCurve.pivotcurrency

	  
	  /** 
	   * Builds zero coupon curve using the curve itself as discount currency.
	   * @param refinance spread on float rate
	   */
	  def getZC(spread : YieldParameter) : DiscountCurve = {
	    require (spread != null)
		  /**
		   * initialize empty containers (sorted tree)
		   */
		  var ZC : TreeMap[JPeriod, Double] = TreeMap.empty
		  var ZCspread : TreeMap[JPeriod, Double] = TreeMap.empty
		
		  /**
		   * spot zero coupon = 1.00
		   */
		  ZC ++= Map(zcperiods(0) -> 1.00)
		  
		  /**
		   * zero coupon spread is unadjusted
		   */
		  ZCspread ++= zcperiods.map(m => (m._2, spread.value(m._2)))
		  
		  /**
		   * cash rate to compute zero coupon < 12 months.
		   */
		  cashrange foreach {cr => 
		    val m = cr._1; 
		    val p = cr._2;
			val zcXm = 1 / (1 + (cash.value(p) + ZCspread(p) - bs3m6madjust(m)) * floatfraction * m / 12)
	  	  	ZC ++= Map(p -> zcXm)}
		  
		  var duration = if (fixperiod >= 12) 0.0
				  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * fixdaycounts(m._1)) toList).sum
		  
		  /**
		   * swap rate to compute zero coupon >= 1year 
		   */
		  swaprange foreach { sr => 
		    val m = sr._1; 
		    val p = sr._2;
		    val realrate = swap.value(p) + (ZCspread(p) - bs3m6madjust(m)) * floatfraction / fixfraction;
		    val zcXm = (1 - realrate * duration) / (1 + realrate * fixdaycounts(m)) 
		    ZC ++= Map(p -> zcXm)
		    duration += zcXm * fixdaycounts(m)}
		  
		  
		  /**
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = new SplineEExtrapolation(valuedate, ZC, 1)
		  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
		  
		  new DiscountCurve(currency, ZCvector, ZCspdvector, cash.floatindex.dayCounter, fx)
	  }

	  /** 
	   * Builds zero coupon curve using external curve as discount currency.
	   * Either external curve or this curve must be basis swap pivot currency (ie USD)
	   */
	  def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {
	    require (refincurve != null && refinZC != null)
		  /** 
		   * initialize empty containers (sorted tree)
		   */ 
		  var ZC : TreeMap[JPeriod, Double] = TreeMap.empty
		  var ZCspread : TreeMap[JPeriod, Double] = TreeMap.empty
	
		  /**
		   * annual daycount fraction for discount curve
		   */
		  val floatfraction2 = refincurve.swap.floatindex.dayCounter().annualDayCount()
		  
		  /**
		   * spot zero coupon = 1.00
		   */
		  ZC ++= Map(zcperiods(0) -> 1.00)
		  
		  /**
		   * initialize ccy basis swap 
		   */
		  val bsccy = zcperiods.map(p => (p._1, 
		      if (ispivotcurrency) -refincurve.basis.value(p._2) 
		      else basis.value(p._2)))
				  	
		  /**
		   * initialize refinance spread
		   */
		  val refinspread = zcperiods.map(p => (p._1, refinZC.discountspread.value(p._2)))
		  val refinZCvector = zcperiods.filter(p => p._1 % fixperiod == 0).map(p => (p._1, refinZC.zc.value(p._2)))
		  def durationfunc(i:Int):Double = {if (i == 0) 0.0 else durationfunc(i - fixperiod) + refinZCvector(i) * floatdaycounts(i)}
		  val refinduration = refinZCvector.filter(m => m._1 % fixperiod == 0).map(v => (v._1, durationfunc(v._1)))
		  
		  /**
		   * using cash rate to compute zero coupon < 12 months.
		   */
		  cashrange foreach { cr => 
		    val m = cr._1; 
		    val p = cr._2;
		    val spd = 	if (ispivotcurrency) (bsccy(m) + refinspread(m)) * floatfraction2 / floatfraction
		    			else bsccy(m) + refinspread(m) * floatfraction / floatfraction2
//		    val bs3m6m = if (m == 3) 0.0 else tenorbasis.value(p)
		    val bs3m6m = bs3m6madjust(m)
		    val zcXm = 1 / (1 + (cash.value(p) + spd - bs3m6m) * floatfraction * m / 12)
		    ZCspread ++= Map(p -> spd)
		    ZC ++= Map(p -> zcXm)
		  	}
	
		  var fixduration = if (fixperiod >= 12) 0.0
				  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * fixdaycounts(m._1)) toList).sum
				  		
		  var floatduration = if (fixperiod >= 12) 0.0
				  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * floatdaycounts(m._1)) toList).sum
		  
		  /**
		   * using swap rate to compute zero coupon >= 1year 
		   */
		  swaprange foreach { sr => 
		    val m = sr._1; 
		    val p = sr._2;
		  	val fduration = if (m <= fixperiod) floatfraction else floatduration
		  	val rduration = if (m <= fixperiod) floatfraction2 else refinduration(m - fixperiod)
			val zcspd = if (ispivotcurrency) (bsccy(m) + refinspread(m)) * rduration / fduration
					 	else bsccy(m) + refinspread(m) * rduration / fduration
		  	val tbs = bs3m6madjust(m) * (if (ispivotcurrency) rduration / fduration else 1.0)
			ZCspread ++= Map(p -> zcspd)
		  	val realrate = swap.value(p) + (zcspd - tbs) * floatfraction / fixfraction
		  	val zcXm = (1 - realrate * fixduration) / (1 + realrate * fixdaycounts(m))
			ZC ++= Map(p -> zcXm)
			fixduration += zcXm * fixdaycounts(m)
			floatduration += zcXm * floatdaycounts(m)
			}	
		  
		  /**
		   * Construct new discount curve object.
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = new SplineEExtrapolation(valuedate, ZC, 1)
		  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
		  
		  new DiscountCurve(currency, ZCvector, ZCspdvector, cash.floatindex.dayCounter, fx)
	    
	  }
  
	  def this(cash:CashCurve, swap:SwapCurve, basis:BasisSwapCurve, tenorbasis:TenorBasisSwapCurve) = this(cash, swap, basis, tenorbasis, 0.0)

}




object LiborDiscountCurve {
  import squantlib.parameter.yieldparameter.FlatVector
  
	val cashKey = "Cash"
	val swapKey = "Swap"
	val basisKey = "BasisSwap"
	val basis36Key = "BS3M6M"
	val fxKey = "FX"
	val pivotccy = "USD"
	  
	/**
	 * Constructs LiborDiscountCurve from InputParameter per each combination of currency & paramset.
	 * Invalid input parameter sets are ignored.
	 * @param set of InputParameter
	 * @returns map from (Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getcurves(params:Traversable[RateFXParameter]):Iterable[LiborDiscountCurve] = {
	  val conventions:Map[String, RateConvention] = RateConvention.getConvention.filter{case (k, v) => v.useratediscount }
  	  val dateassetgroup = params.groupBy(p => p.asset).filter{case(k, v) => conventions.keySet.contains(k)}
  	  val instrumentgroup = dateassetgroup.map{ case (k, v) => (k, v.groupBy(p => p.instrument))} 
  	  val nonemptyinstruments = instrumentgroup.filter{ case (k, v) => (v.keySet.contains(swapKey))}
  	  
  	  nonemptyinstruments.map{ case (k, v) => {
  		  val conv = conventions(k)
  		  val valuedate = new JDate(v(swapKey).head.paramdate)
  		  def toTreeMap(k:String) = TreeMap(v(k).toSeq.map(p => (new JPeriod(p.maturity), p.value)) :_*)
  		  val swapcurve = conv.swap_constructor(valuedate, toTreeMap(swapKey))
  		  val cashcurve = if (v.keySet.contains(cashKey)) conv.cash_constructor(valuedate, toTreeMap(cashKey))
  		  				  else conv.cash_constructor(new FlatVector(valuedate, swapcurve.rate.value(0)))
  		  val basiscurve = if (v.keySet.contains(basisKey)) conv.basis_constructor(valuedate, toTreeMap(basisKey)) else null
  		  val basis36curve = if (v.keySet.contains(basis36Key)) conv.basis36_constructor(valuedate, toTreeMap(basis36Key)) else null
  		  
  		  if (v.keySet.contains(fxKey)) new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve, v(fxKey).head.value)
  		  else new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve)
  	  	}}
  	}
  
//	/**
//	 * Constructs LiborDiscountCurve from InputParameter per each currency, only with specified paramset.
//	 * Invalid input paramter sets or unmatched paramset ID are ignored.
//	 * @param set of InputParameter
//	 * @returns map from Currency to LiborDiscountCurve
//	 */
//  	def getcurves(params:Traversable[InputParameter], paramset:String):Map[String, LiborDiscountCurve] = 
//  	  getcurves(params.filter(p => p.paramset == paramset)) map { case ((k1, k2), v) => (k1, v) }
  	
} 

