package squantlib.ratecurve

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedSet

import squantlib.parameter._

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.daycounters.DayCounter;
 
import org.apache.commons.math3.analysis.function.Log
import org.apache.commons.math3.analysis.function.Exp


class LiborDiscountCurve (val cash:CashCurve, val swap:SwapCurve, val basis:BasisSwapCurve, val tenorbasis:TenorBasisSwapCurve, val valuedate : JDate) 
extends RateCurve{
  require (List(cash.valuedate, swap.valuedate, basis.valuedate, tenorbasis.valuedate).forall(_ == valuedate)
		&& List(swap.currency, basis.currency, tenorbasis.currency).forall(_ == cash.currency)
		&& cash.floatindex.dayCounter == swap.floatindex.dayCounter)

	  val currency = cash.currency
	  
	  /**
	   * swap specifications
	   * we often use yearly day count factor instead of exact calendar dates, with small potential error
	   */
	  val floattenor = swap.floatindex.tenor().length()	  
	  val fixperiod = 12 / swap.fixperiod.toInteger()
  	  val fixfraction = swap.fixdaycount.annualDayCount()
	  val floatfraction = swap.floatindex.dayCounter().annualDayCount()
	  
	  println("Parameters")
	  println("floattenor : " + floattenor)
	  println("flatperiod : " + fixperiod)
	  println("fixfraction : " + fixfraction)
	  println("floatfraction : " + floatfraction)
	  
	  /**
	   * day count initialization, for swap fixed leg convention. (not to be used for cash rate)
	   */
	  val maxmaturity = JPeriod.months(swap.rate.maxperiod, valuedate).toInt
	  val zcmonths:Seq[Int] = (for (m <- (List(0, 3, 6, 9) ++ (12 until maxmaturity by fixperiod))) yield m).sorted
	  val zcperiods = TreeMap(zcmonths.map(m => (m, new JPeriod(m, TimeUnit.Months))) : _*) 
	  val maturities = TreeMap(zcmonths.map(m => (m, valuedate.add(zcperiods(m)))) : _*) 
	  val fixdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
			  		.map(m => (m, swap.fixdaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
	  val floatdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  				.map(m => (m, swap.floatindex.dayCounter().yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
	  				

	  /**
	   * 3m/6m basis swap calibration is valid in case float leg is semi annual (ccy basis always quarterly)
	   */
	  val bs3m6madjust = zcperiods.map(m => (m._1, if (floattenor <= 3) 0.0 else tenorbasis.value(m._2))) 

	  
	  println("maxmaturity : " + maxmaturity)
	  println("[m, zcmonths, zcperiods, maturities, fixdaycounts, floatdaycounts, bs3m6madj]")
	  for (m <- fixdaycounts.keySet){
	    println(m + ", " + zcperiods(m).toString + ", " + maturities(m).shortDate.toString + ", " + fixdaycounts(m) + ", " + floatdaycounts(m) + ", " + bs3m6madjust(m))
	  }
	  
	  
	  /**
	   * true if this currency is the "pivot" currency for the basis swap, usually USD.
	   * no support for additional pivot currency.
	   */
	  val ispivotcurrency = basis.ispivotcurrency

	  /**
	   * using cash rate to compute zero coupon < 12 months.
	   */
	  val cashrange = zcperiods.filter(m => (m._1 < 12 && m._1 > 0))
	  val swaprange = zcperiods.filter(m => m._1 >= 12)
	  
	  
	  def getZC(spread : TimeVector) : DiscountCurve = {
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
		   * note: we are assuming 3m-9m basis spread = 3m-6m basis spread
		   */
		  cashrange foreach {cr => val m = cr._1; val p = cr._2;
			val bs3m6m = if (m < 6) 0.0 else tenorbasis.value(p)
			val zcXm = 1 / (1 + (cash.value(p) + ZCspread(p) - bs3m6m) * floatfraction * m / 12)
	  	  	ZC ++= Map(p -> zcXm)}
		  
		  var duration = if (fixperiod >= 12) 0.0
				  		else (cashrange.filter(m => m._1 % fixperiod == 0).map(m => ZC(m._2) * fixdaycounts(m._1)) toList).sum
		  
		  /**
		   * swap rate to compute zero coupon >= 1year 
		   * note: we are assuming no basis below 3 months and above 6 months
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

  
  def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {
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
	   * note: we are assuming 3m-9m basis spread = 3m-6m basis spread
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
	   * note: we are assuming no basis below 3 months and above 6 months
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
	  		println("m: " + m + " zcspd:" + zcspd + " tbs: " + tbs + " bsccy:" + bsccy(m) + " refinspread: " + refinspread(m) + " refinduration:"+rduration+ " floatduration:" + fduration)
			ZC ++= Map(p -> zcXm)
			fixduration += zcXm * fixdaycounts(m)
			floatduration += zcXm * floatdaycounts(m)
		  }	
	  
	  /**
	   * Construct new discount curve object.
	   * ZC vector is spline interpolation with exponential extrapolation
	   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
	   */
	  ZC foreach { z => println("m=" + z._1 + " value=" + z._2)}
	  ZCspread foreach { z => println("m=" + z._1 + " value=" + z._2)}
	  
	  val ZCvector = new SplineEExtrapolation(valuedate, ZC, 1)
	  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
	  new DiscountCurve(ZCvector, ZCspdvector)
    
  }
  
}