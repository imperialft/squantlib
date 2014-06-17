package net.squantlib.model.rates

import scala.collection.immutable.{TreeMap, SortedSet}
import net.squantlib.model.yieldparameter.{FlatVector, YieldParameter, SplineEExtrapolation, SplineNoExtrapolation, LinearNoExtrapolation}
import org.jquantlib.time.{Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.DayCounter;
import net.squantlib.model.rates.convention.RateConvention
import net.squantlib.util.Date
 
  /**
   * Libor discounting model
   * - 3m/6m basis is paid semiannually instead of quarterly (small error)
   * - zero rate volatility (model assumption)
   * - no 3m-Xm basis for X < 6 (implied by ZC interpolation 3m & 6m)
   * - no 6m-Xm basis for X > 6 (implied by ZC interpolation 6m & 12m)
   */
case class NDSDiscountCurve (nds:NDSCurve, pivotDiscount:DiscountCurve, pivotTenorBS:TenorBasisSwapCurve, fx:Double, vol:Option[RateVolatility]) 
extends DiscountableCurve {

	assert(nds.floatCurrency.code == "USD")
	assert(pivotDiscount.currency.code == "USD")

	  val currency = nds.currency
	  val valuedate = nds.valuedate
	  
	  
	  /**
	   * swap specifications
	   * we use yearly day count factor instead of exact calendar dates as estimate in few cases, with small potential error
	   */
	  val floattenor = nds.floatindex.tenor.length
	  val fixperiod = 12 / nds.fixperiod.toInteger()
  	  val fixfraction = nds.fixdaycount.annualDayCount()
	  val floatfraction = nds.floatindex.dayCounter().annualDayCount()
	  
	  def pivotTenorPV(p:qlPeriod):Double = pivotDiscount.duration(p, pivotTenorBS.shortindex.tenor, pivotTenorBS.shortindex.dayCounter, pivotTenorBS.shortindex.businessDayConvention) * pivotTenorBS(p)
	  
	  /**
	   * day count initialization, for swap fixed leg convention. (not to be used for cash rate)
	   */
	  val maxmaturity = valuedate.months(nds.rate.maxperiod).toInt
	  
	  val zcmonths:List[Int] = (for (m <- ((0 to maxmaturity by fixperiod).toList)) yield m).sorted
	  
	  val zcperiods = TreeMap(zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))) : _*) 
	  
	  val maturities = TreeMap(zcmonths.map(m => (m, valuedate.add(zcperiods(m)))) : _*) 
	  
	  val fixdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
			  		.map(m => (m, Date.daycount(maturities(m-fixperiod), maturities(m), nds.fixdaycount))) : _*)
			  		
	  val floatdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  				.map(m => (m, Date.daycount(maturities(m-fixperiod), maturities(m), nds.floatindex.dayCounter))) : _*)
	  				
	  val ndsrange = zcperiods.filter{case (m, p) => m > 0}
	
	  /**
	   * true if this currency is the "pivot" currency for the basis swap, usually USD.
	   * no support for additional pivot currency.
	   */
	  val ispivotcurrency = false

	  
	  /** 
	   * Builds zero coupon curve using the curve itself as discount currency.
	   * @param refinance spread on float rate
	   */
	  def getZC(spread : YieldParameter) : DiscountCurve = {
	    require (spread != null)
		  /**
		   * initialize empty containers (sorted tree)
		   */
		  var ZC : TreeMap[qlPeriod, Double] = TreeMap.empty
		  var ZCspread : TreeMap[qlPeriod, Double] = TreeMap.empty
		
		  /**
		   * spot zero coupon = 1.00
		   */
		  ZC ++= Map(zcperiods(0) -> 1.00)
		  
		  /**
		   * zero coupon spread is unadjusted
		   */
		  ZCspread ++= zcperiods.map{case (m, p) => (p, spread(p))}
		  
		  var duration = 0.0
		  
		  /**
		   * swap rate to compute zero coupon >= 1year 
		   */
		  ndsrange foreach { case (m, p) => 
		    val realrate = nds(p) + ZCspread(p) * floatfraction / fixfraction
		    val zcXm = (1 - realrate * duration + pivotTenorPV(p)) / (1 + realrate * fixdaycounts(m)) 
		    ZC ++= Map(p -> zcXm)
		    duration += zcXm * fixdaycounts(m)
		  }
		  
		  
		  /**
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = SplineEExtrapolation(valuedate, ZC, 1)
		  val ZCspdvector = SplineNoExtrapolation(valuedate, ZCspread, 2)
		  
		  DiscountCurve(currency, ZCvector, ZCspdvector, fx, vol)
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
		  var ZC : TreeMap[qlPeriod, Double] = TreeMap.empty
		  var ZCspread : TreeMap[qlPeriod, Double] = TreeMap.empty
	
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
		  val bsccy = zcperiods.map{case(m, p) => (m, 
		      if (refincurve.currency == nds.floatCurrency) 0.0
		      else -refincurve.basis(p))}
				  	
		  /**
		   * initialize refinance spread
		   */
		  val refinspread = zcperiods.map(p => (p._1, refinZC.discountspread(p._2)))
		  val refinZCvector = zcperiods.filter(p => p._1 % fixperiod == 0).map(p => (p._1, refinZC.zc(p._2)))
		  def durationfunc(i:Int):Double = {if (i == 0) 0.0 else durationfunc(i - fixperiod) + refinZCvector(i) * floatdaycounts(i)}
		  val refinduration = refinZCvector.filter(m => m._1 % fixperiod == 0).map(v => (v._1, durationfunc(v._1)))
		  
		  var fixduration = 0.0
		  var floatduration = 0.0
		  
		  /**
		   * using swap rate to compute zero coupon >= 1year 
		   */
		  ndsrange foreach { case(m, p) => 
		  	val fduration = if (m <= fixperiod) floatfraction else floatduration
		  	val rduration = if (m <= fixperiod) floatfraction2 else refinduration(m - fixperiod)
			val zcspd = if (ispivotcurrency) (bsccy(m) + refinspread(m)) * rduration / fduration
					 	else bsccy(m) + refinspread(m) * rduration / fduration
			ZCspread ++= Map(p -> zcspd)
		  	val realrate = nds(p) + zcspd * floatfraction / fixfraction
		  	val zcXm = (1 + pivotTenorPV(p) - realrate * fixduration) / (1 + realrate * fixdaycounts(m))
			ZC ++= Map(p -> zcXm)
			fixduration += zcXm * fixdaycounts(m)
			floatduration += zcXm * floatdaycounts(m)
			}
		  
		  /**
		   * Construct new discount curve object.
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = SplineEExtrapolation(valuedate, ZC, 1)
		  val ZCspdvector = SplineNoExtrapolation(valuedate, ZCspread, 2)
		  
		  DiscountCurve(currency, ZCvector, ZCspdvector, fx, vol)
	    
	  }
	  
	  override def shiftRate(shift: (Double, Double) => Double):NDSDiscountCurve = NDSDiscountCurve(nds.shifted(shift), pivotDiscount, pivotTenorBS, fx, null)
	  override def multFX(v: Double):NDSDiscountCurve = NDSDiscountCurve(nds, pivotDiscount, pivotTenorBS, fx * v, null)
  
}



