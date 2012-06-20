package squantlib.ratecurve

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedSet

import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.parameter.yieldparameter.SplineEExtrapolation
import squantlib.parameter.yieldparameter.SplineNoExtrapolation

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.daycounters.DayCounter;
 

  /**
   * Libor discounting model
   * - 3m/6m basis is paid semiannually instead of quarterly (small error)
   * - zero rate volatility (model assumption)
   * - no 3m-Xm basis for X < 6 (implied by ZC interpolation 3m & 6m)
   * - no 6m-Xm basis for X > 6 (implied by ZC interpolation 6m & 12m)
   */
class LiborDiscountCurve (val cash:CashCurve, val swap:SwapCurve, val basis:BasisSwapCurve, val tenorbasis:TenorBasisSwapCurve, val valuedate : JDate, val fx : Double = 0) 
extends RateCurve{
  require (List(cash.valuedate, swap.valuedate, basis.valuedate, tenorbasis.valuedate).forall(_ == valuedate)
		&& List(swap.currency, basis.currency, tenorbasis.currency).forall(_ == cash.currency)
		&& cash.floatindex.dayCounter == swap.floatindex.dayCounter)

	  val currency = cash.currency
	  val basedaycount = cash.floatindex.dayCounter
	  
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
	  val bs3m6madjust = zcperiods.map(m => (m._1, m._1 match { case n if n < swapstart && n < 6 => 0.0
															    case n if n < swapstart && n >= 6 => tenorbasis.value(m._2)
															    case n if n >= swapstart && floattenor <= 3 => 0.0
															    case _ => tenorbasis.value(m._2) }))
	  
	  
	  /**
	   * true if this currency is the "pivot" currency for the basis swap, usually USD.
	   * no support for additional pivot currency.
	   */
	  val ispivotcurrency = basis.ispivotcurrency

	  
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
		  cashrange foreach {cr => val m = cr._1; val p = cr._2;
			val zcXm = 1 / (1 + (cash.value(p) + ZCspread(p) - bs3m6madjust(m)) * floatfraction * m / 12)
	  	  	ZC ++= Map(p -> zcXm)}
		  
		  var duration = if (fixperiod >= 12) 0.0
				  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * fixdaycounts(m._1)) toList).sum
		  
		  /**
		   * swap rate to compute zero coupon >= 1year 
		   */
		  swaprange foreach { sr => val m = sr._1; val p = sr._2;
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
		  new DiscountCurve(ZCvector, ZCspdvector)
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
		  val bsccy = zcperiods.map(p => (p._1, if (ispivotcurrency) -refincurve.basis.value(p._2) else basis.value(p._2)))
				  	
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
		  cashrange foreach { cr => val m = cr._1; val p = cr._2;
		    val spd = 	if (ispivotcurrency) (bsccy(m) + refinspread(m)) * floatfraction2 / floatfraction
		    			else bsccy(m) + refinspread(m) * floatfraction / floatfraction2
		    val bs3m6m = if (m == 3) 0.0 else tenorbasis.value(p)
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
		  swaprange foreach { sr => val m = sr._1; val p = sr._2;
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
		  new DiscountCurve(ZCvector, ZCspdvector)
	    
	  }
  
}