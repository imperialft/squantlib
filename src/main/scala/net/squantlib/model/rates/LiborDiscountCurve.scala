package net.squantlib.model.rates

import net.squantlib.model.yieldparameter._
import net.squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.DayCounter;
import net.squantlib.model.rates.convention.RateConvention
import scala.annotation.tailrec
import scala.collection.breakOut

  /**
   * Libor discounting model
   * - 3m/6m basis is paid semiannually instead of quarterly (small error)
   * - zero rate volatility (model assumption)
   * - no 3m-Xm basis for X < 6 (implied by ZC interpolation 3m & 6m)
   * - no 6m-Xm basis for X > 6 (implied by ZC interpolation 6m & 12m)
   */
case class LiborDiscountCurve (
    cash:CashCurve, 
    swap:SwapCurve, 
    basis:BasisSwapCurve, 
    tenorbasis:TenorBasisSwapCurve, 
    fx:Double, 
    vol:Option[RateVolatility]) extends RateCurve {
  
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
	  val maxmaturity = valuedate.months(swap.rate.maxperiod).toInt
	  
	  val zcmonths = (for (m <- (List(0, 3, 6, 9) ++ (12 to maxmaturity by fixperiod))) yield m).sorted
	  
	  val zcperiods:Map[Int, qlPeriod] = zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))) (breakOut)
	  
	  val sortedperiods = zcperiods.toList.sortBy(_._1)
	  
	  val maturities:Map[Int, Date] = zcmonths.map(m => (m, valuedate.add(zcperiods(m)))) (breakOut)
	  
	  val fixdaycounts:Map[Int, Double] = zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
			  		.map(m => (m, Date.daycount(maturities(m-fixperiod), maturities(m), swap.fixdaycount)))(breakOut)
			  		
	  val floatdaycounts:Map[Int, Double] = zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  				.map(m => (m, Date.daycount(maturities(m-fixperiod), maturities(m), swap.floatindex.dayCounter)))(breakOut)
	  				
	  /**
	   * using cash rate to compute zero coupon < 12 months.
	   */
	  val swapstart = 12;				

	  /**
	   * 3m/6m basis swap calibration is valid in case float leg is semi annual (ccy basis always quarterly)
	   */
	  val bs3m6madjust = if (tenorbasis == null && floattenor > 3) null 
	  					 else zcperiods.map{
	  					   case (m, p) => (m, m match { 
	  					    case n if n < swapstart && n < 6 => 0.0
						    case n if n < swapstart && n >= 6 => if (tenorbasis == null) 0.0 else tenorbasis(p)
						    case n if n >= swapstart && floattenor <= 3 => 0.0
						    case _ => tenorbasis(p) })}
  
	  /**
	   * true if this currency is the "pivot" currency for the basis swap, usually USD.
	   * no support for additional pivot currency.
	   */
	  val ispivotcurrency = swap.currency == BasisSwapCurve.pivotcurrency
	  
	  override def isPivotDiscountable:Boolean = (ispivotcurrency || basis != null) && swap != null && cash != null && (tenorbasis != null || floattenor == 3)

	  
	  /** 
	   * Builds zero coupon curve using the curve itself as discount currency.
	   * @param refinance spread on float rate
	   */
	  def getZC(spread : YieldParameter) : DiscountCurve = {
	    
	    @tailrec def zcRec(dates:List[(Int, qlPeriod)], zc:List[Double], duration:Double):List[Double] = {
	      if (dates.isEmpty) zc.reverse
	      else dates.head match {
	        case (m, p) if m == 0 => zcRec(dates.tail, 1.00 :: zc, duration)
	        
	        case (m, p) if m < swapstart =>
	          val c = cash(p) * 1.0
	          val spr = spread(p) * 1.0
	          val bs36 = bs3m6madjust(m) * 1.0
	          val flfunc = floatfraction * 1.0
	          val zcXm:Double = 1 / (1 + (c + spr - bs36) * flfunc * m / 12)
	          val newDur:Double = if (m % fixperiod == 0) zcXm * fixdaycounts(m) else 0.0
	          zcRec(dates.tail, zcXm :: zc, duration + newDur)
	          
	        case (m, p) =>
	          val realrate = swap(p) + (spread(p) - bs3m6madjust(m)) * floatfraction / fixfraction
	          val zcXm = (1 - realrate * duration) / (1 + realrate * fixdaycounts(m)) 
	          zcRec(dates.tail, zcXm :: zc, duration + zcXm * fixdaycounts(m))
	      }
	    }
	    
	    val zcvalues = zcRec(sortedperiods, List.empty, 0.0)
	    val zc:Map[qlPeriod, Double] = (sortedperiods.unzip._2 zip zcvalues)(breakOut)
	    val ZCvector = SplineEExtrapolation(valuedate, zc, 1)
	  
	    DiscountCurve(currency, ZCvector, spread, fx, vol)
	  }

	  /** 
	   * Builds zero coupon curve using external curve as discount currency.
	   * Either external curve or this curve must be basis swap pivot currency (ie USD)
	   */
	  def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {

		/**
		 * annual daycount fraction for discount curve
		 */
		val floatfraction2 = refincurve.swap.floatindex.dayCounter().annualDayCount()
		
		/**
		 * initialize ccy basis swap 
		 */
		val bsccy = zcperiods.map{case(m, p) => (m, 
		    if (ispivotcurrency) 
		      -refincurve.basis(p) 
	        else 
	          basis(p)
          )}
				  	
		/**
		 * initialize refinance spread
		 */
		val refinspread = zcperiods.map(p => (p._1, refinZC.discountspread(p._2)))
		val refinZCvector = zcperiods.filter(p => p._1 % fixperiod == 0).map(p => (p._1, refinZC.zc(p._2)))
		def durationfunc(i:Int):Double = {if (i == 0) 0.0 else durationfunc(i - fixperiod) + refinZCvector(i) * floatdaycounts(i)}
		val refinduration = refinZCvector.filter(m => m._1 % fixperiod == 0).map(v => (v._1, durationfunc(v._1)))
	    
	    @tailrec def zcRec(dates:List[(Int, qlPeriod)], zc:List[Double], zcspread:List[Double], fixduration:Double, floatduration:Double):(List[Double], List[Double]) = {
	      if (dates.isEmpty) (zc, zcspread)
	      else dates.head match {
	        case (m, p) if m == 0 => 
	          val spd = if (ispivotcurrency) (bsccy(m) + refinspread(m)) * floatfraction2 / floatfraction
		    			else bsccy(m) + refinspread(m) * floatfraction / floatfraction2	          
	          zcRec(dates.tail, zc :+ 1.00, zcspread :+ spd, fixduration, floatduration)
	        
	        case (m, p) if m < swapstart => 
	          val spd = if (ispivotcurrency) (bsccy(m) + refinspread(m)) * floatfraction2 / floatfraction
		    			else bsccy(m) + refinspread(m) * floatfraction / floatfraction2
		      val bs3m6m = bs3m6madjust(m)
		      val zcXm = 1 / (1 + (cash(p) + spd - bs3m6m) * floatfraction * m / 12)
		      val newfix = if (m % fixperiod == 0) zcXm * fixdaycounts(m) else 0.0
		      val newfloat = if (m % fixperiod == 0) zcXm * floatdaycounts(m) else 0.0
		      zcRec(dates.tail, zc :+ zcXm, zcspread :+ spd, fixduration + newfix, floatduration + newfloat)
		      
	        case (m, p) =>
	          val fduration = if (m <= fixperiod) floatfraction else floatduration
		  	  val rduration = if (m <= fixperiod) floatfraction2 else refinduration(m - fixperiod)
			  val zcspd = if (ispivotcurrency) (bsccy(m) + refinspread(m)) * rduration / fduration
					 	else bsccy(m) + refinspread(m) * rduration / fduration
		  	  val tbs = bs3m6madjust(m) * (if (ispivotcurrency) rduration / fduration else 1.0)
		  	  val realrate = swap(p) + (zcspd - tbs) * floatfraction / fixfraction
		  	  val zcXm = (1 - realrate * fixduration) / (1 + realrate * fixdaycounts(m))
		  	  zcRec(dates.tail, zc :+ zcXm, zcspread :+ zcspd, fixduration + zcXm * fixdaycounts(m), floatduration + zcXm * floatdaycounts(m))
	      }
	    }
	    
	    val (zcvalues, spdvalues) = zcRec(sortedperiods, List.empty, List.empty, 0.0, 0.0)
	    val zc:Map[qlPeriod, Double] = (sortedperiods.unzip._2 zip zcvalues)(breakOut)
	    val spd:Map[qlPeriod, Double] = (sortedperiods.unzip._2 zip spdvalues)(breakOut)
	    
	    val zcvector = SplineEExtrapolation(valuedate, zc, 1)
		val spdvector = SplineNoExtrapolation(valuedate, spd, 2)
	  
	    DiscountCurve(currency, zcvector, spdvector, fx, vol)
	    
	  }
	  
	  override def shiftRate(shift: (Double, Double) => Double):LiborDiscountCurve = LiborDiscountCurve(cash.shifted(shift), swap.shifted(shift), basis, tenorbasis, fx, vol)
	  
	  override def multFX(v: Double):LiborDiscountCurve = LiborDiscountCurve(cash, swap, basis, tenorbasis, fx * v, vol)

}




