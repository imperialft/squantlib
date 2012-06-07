package squantlib.ratecurve

import scala.collection.immutable.TreeMap

import squantlib.parameter._

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.daycounters.DayCounter;
 
import org.apache.commons.math3.analysis.function.Log
import org.apache.commons.math3.analysis.function.Exp


class LiborDiscountCurve (val cash:CashCurve, val swap:SwapCurve, val basis:BasisSwapCurve, val tenorbasis:TenorBasisSwapCurve, val valuedate : JDate) 
extends RateCurve{
  require (List(cash.valuedate, swap.valuedate, basis.valuedate, tenorbasis.valuedate).forall(_ == valuedate))
  
	def getZC(spread : TimeVector) : DiscountCurve = {
	  /**
	   * initialize empty containers (sorted tree)
	   */
	  var ZC : TreeMap[JPeriod, Double] = TreeMap.empty
	  var ZCspread : TreeMap[JPeriod, Double] = TreeMap.empty

	  /**
	   * initialize day count fractions
	   */
	  val floatmult = swap.floatIndex.dayCounter().annualDayCount()
	  val fixmult = swap.fixDaycount.annualDayCount()
	  val floattenor = swap.floatIndex.tenor().length()	  
	  val fixperiod = 12 / swap.fixPeriod.toInteger()
	  
	  /**
	   * initialize maturities and daycounts.
	   * Daycount is for swap fixed leg convention. (not to be used for cash rate)
	   */
	  val maxmaturity = JPeriod.months(swap.rate.maxPeriod, valuedate).toInt
	  val zcmonths = for (m <- (List(0, 3, 6, 9) ++ (12 until maxmaturity by fixperiod))) yield m
	  val periods = zcmonths.map(m => (m, new JPeriod(m, TimeUnit.Months))) toMap
	  val maturities = zcmonths.map(m => (m, valuedate.add(periods(m)))) toMap
	  val dcf = zcmonths.filter(_ % fixperiod == 0).map(m => (m, swap.fixDaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) toMap
	  val bs3m6m = periods.map(m => (m._1, if (floattenor <= 3) 0.0 else tenorbasis.value(m._2))) toMap
	  
	  
	  /**
	   * spot zero coupon = 1.00
	   */
	  ZC ++= Map(periods(0) -> 1.00)
	  
	  
	  /**
	   * zero coupon spread is unadjusted
	   */
	  ZCspread ++= periods.map(m => (m._2, spread.value(m._2)))
	  
	  /**
	   * using cash rate to compute zero coupon < 12 months.
	   * note: we are assuming 3m-9m basis spread = 3m-6m basis spread
	   */
	  val cashrange = periods.filter(m => (m._1 < 12 && m._1 > 0))
	  
	  cashrange foreach {m =>
	    val zcs = ZCspread(m._2)
	  	val lib = cash.value(m._2);
		val bs3m6m = if (m._1 < 6) 0.0 else tenorbasis.value(m._2)
  	  	ZC ++= Map(m._2 -> 1 / (1 + (lib + zcs - bs3m6m) * floatmult * m._1 / 12))}
	  
	  var duration = if (fixperiod >= 12) 0.0
			  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * dcf(m._1)) toList).sum
	  
	  /**
	   * using swap rate to compute zero coupon >= 1year 
	   * note: we are assuming no basis below 3 months and above 6 months
	   */
	  val swaprange = periods.filter(m => m._1 >= 12)
	  
	  swaprange foreach { m =>
	    val realrate = swap.value(m._2) + (ZCspread(m._2) - bs3m6m(m._1)) * floatmult / fixmult;
	    val zcXm = (1 - realrate * duration) / (1 + realrate * dcf(m._1)) 
	    ZC ++= Map(m._2 -> zcXm)
	    duration += zcXm * dcf(m._1)}
	  
	  
	  /**
	   * Construct new discount curve object.
	   * ZC vector is spline interpolation with exponential extrapolation
	   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
	   */
	  val ZCvector = new SplineEExtrapolation(valuedate, ZC)
	  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
	  new DiscountCurve(ZCvector, ZCspdvector)
  }

  
  def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {
	  /** 
	   * initialize empty containers (sorted tree)
	   */ 
	  var ZC : TreeMap[JPeriod, Double] = TreeMap.empty
	  var ZCspread : TreeMap[JPeriod, Double] = TreeMap.empty

	  /**
	   * initialize day count fractions
	   */
	  val fixmult = swap.fixDaycount.annualDayCount()
	  val floatmult = swap.floatIndex.dayCounter().annualDayCount()
	  val floatmult2 = refincurve.swap.floatIndex.dayCounter().annualDayCount()
	  val floattenor = swap.floatIndex.tenor().length()	  
	  val fixperiod = 12 / swap.fixPeriod.toInteger()
	  
	  /**
	   * initialize maturities and daycounts.
	   * Daycount is for swap fixed leg convention. (not to be used for cash rate)
	   */
	  val maxmaturity = (JPeriod.months(swap.rate.maxPeriod, valuedate)).toInt
	  val zcmonths = for (m <- (List(0, 3, 6, 9) ++ (12 to maxmaturity by fixperiod))) yield m
	  val periods = zcmonths.map(m => (m, new JPeriod(m, TimeUnit.Months))) toMap
	  val maturities = periods.map(p => (p._1, valuedate.add(p._2))) toMap
	  
	  val fixdaycounts = zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  		.map(m => (m, swap.fixDaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) toMap

	  val floatdaycounts = zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  		.map(m => (m, swap.floatIndex.dayCounter().yearFraction(maturities(m-fixperiod), maturities(m)))) toMap

	  val bs3m6m = periods.map(m => (m._1, if (floattenor <= 3) 0.0 else tenorbasis.value(m._2))) toMap
	  		
	  /**
	   * spot zero coupon = 1.00
	   */
	  ZC ++= Map(periods(0) -> 1.00)
	  
	  /**
	   * initialize ccy basis swap
	   */
	  val bsccy = periods.map(p => (p._1, if (basis.ispivotcurrency) -refincurve.basis.value(p._2) else basis.value(p._2))) toMap
			  	
	  /**
	   * initialize refinance spread
	   */
	  val refinspread = periods.filter(p => p._1 % fixperiod == 0).map(p => (p._1, refinZC.discountspread.value(p._2))) toMap
	  val refinZCvector = periods.filter(p => p._1 % fixperiod == 0).map(p => (p._1, refinZC.zc.value(p._2))) toMap
	  def durationfunc(i:Int):Double = {if (i == 0) 0.0 else durationfunc(i - fixperiod) + refinZCvector(i) * floatdaycounts(i)}
	  val refinduration = (for (v <- refinZCvector.keySet) yield (v, durationfunc(v))) toMap
	  
	  /**
	   * using cash rate to compute zero coupon < 12 months.
	   * note: we are assuming 3m-9m basis spread = 3m-6m basis spread
	   */
	  val cashrange = periods.filter(p => p._1 < 12).filter(p => p._1 > 0)
	  
	  cashrange foreach { m =>
	    val spd = if (basis.ispivotcurrency) (bsccy(m._1) + refinspread(m._1)) * floatmult2 / floatmult
	    		else bsccy(m._1) + refinspread(m._1) * floatmult / floatmult2
	    val bs3m6m = if (m == 3) 0.0 else tenorbasis.value(m._2)
	    val zcXm = 1 / (1 + (cash.value(m._2) + spd - bs3m6m) * floatmult * m._1 / 12)
	    ZCspread ++= Map(m._2 -> spd)
	    ZC ++= Map(m._2 -> zcXm)
	  	}

	  var fixduration = if (fixperiod >= 12) 0.0
			  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * fixdaycounts(m._1)) toList).sum
			  		
	  var floatduration = if (fixperiod >= 12) 0.0
			  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * floatdaycounts(m._1)) toList).sum
	  
	  /**
	   * using swap rate to compute zero coupon >= 1year 
	   * note: we are assuming no basis below 3 months and above 6 months
	   */
	  val swaprange = periods.filter(p => p._1 >= 12)
	  
	  swaprange foreach { m =>
	  		val zcs = ZCspread(m._2)
	  		val bs = bsccy(m._1)
			val floatduration2 = refinduration(m._1)
			val zcspd = if (basis.ispivotcurrency) (bs + refinspread(m._1)) * floatduration2 / floatduration
					else bs + refinspread(m._1) * floatduration2 / floatduration
	  		val tbs = bs3m6m(m._1) * (if (basis.ispivotcurrency) floatduration2 / floatduration else 1.0)
	  		val realrate = swap.value(m._2) + (zcs - tbs) * floatmult / fixmult
	  		val zcXm = (1 - realrate * fixduration) / (1 + realrate * fixdaycounts(m._1))
		  	ZCspread ++= Map(m._2 -> zcspd)
			ZC ++= Map(m._2 -> zcXm)
			fixduration += zcXm * fixdaycounts(m._1)
			floatduration += zcXm * floatdaycounts(m._1)
		  }	

	  
	  /**
	   * Construct new discount curve object.
	   * ZC vector is spline interpolation with exponential extrapolation
	   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
	   */
	  val ZCvector = new SplineEExtrapolation(valuedate, ZC)
	  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
	  new DiscountCurve(ZCvector, ZCspdvector)
    
  }
  
}