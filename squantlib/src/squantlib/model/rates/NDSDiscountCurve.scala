package squantlib.model.rates

import scala.collection.immutable.{TreeMap, SortedSet}
import squantlib.model.yieldparameter.{FlatVector, YieldParameter, SplineEExtrapolation, SplineNoExtrapolation, LinearNoExtrapolation}
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.DayCounter;
import squantlib.database.schemadefinitions.RateFXParameter
import squantlib.setting.RateConvention

 
  /**
   * Libor discounting model
   * - 3m/6m basis is paid semiannually instead of quarterly (small error)
   * - zero rate volatility (model assumption)
   * - no 3m-Xm basis for X < 6 (implied by ZC interpolation 3m & 6m)
   * - no 6m-Xm basis for X > 6 (implied by ZC interpolation 6m & 12m)
   */
case class NDSDiscountCurve (nds:NDSCurve, pivotDiscount:DiscountCurve, pivotTenorBS:TenorBasisSwapCurve, fx:Double) 
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
	  val maxmaturity = qlPeriod.months(nds.rate.maxperiod, valuedate).toInt
	  
	  val zcmonths:Seq[Int] = (for (m <- ((0 to maxmaturity by fixperiod).toList)) yield m).sorted
	  
	  val zcperiods = TreeMap(zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))) : _*) 
	  
	  val maturities = TreeMap(zcmonths.map(m => (m, valuedate.add(zcperiods(m)))) : _*) 
	  
	  val fixdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
			  		.map(m => (m, nds.fixdaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
			  		
	  val floatdaycounts = TreeMap(zcmonths.filter(_ % fixperiod == 0).filter(_ >= fixperiod)
	  				.map(m => (m, nds.floatindex.dayCounter().yearFraction(maturities(m-fixperiod), maturities(m)))) : _*)
	  				
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
		  
		  new DiscountCurve(currency, ZCvector, ZCspdvector, fx)
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
		  
		  new DiscountCurve(currency, ZCvector, ZCspdvector, fx)
	    
	  }
	  
	  override def shiftRate(shift: (Double, Double) => Double):NDSDiscountCurve = NDSDiscountCurve(nds.shifted(shift), pivotDiscount, pivotTenorBS, fx)
	  override def multFX(v: Double):NDSDiscountCurve = NDSDiscountCurve(nds, pivotDiscount, pivotTenorBS, fx * v)
  
}




object NDSDiscountCurve {
  
	val ndsKey = "NDS"
	val fxKey = "FX"

	/**
	 * Constructs LiborDiscountCurve from InputParameter per each combination of currency & paramset.
	 * Invalid input parameter sets are ignored.
	 * @param set of InputParameter
	 * @returns map from (Currency, ParamSet) to LiborDiscountCurve
	 */
  	def apply(params:Set[RateFXParameter], pivotDiscount:DiscountCurve, pivotTenorBS:TenorBasisSwapCurve, valuedate:qlDate):Set[NDSDiscountCurve] = {
    
	  val currencies = RateConvention.toMap.filter{case (k, v) => v.useNDSdiscount }.keySet
	  
  	  val nonemptyinstruments:Map[String, Map[String, Map[qlPeriod, Double]]] = 
 	    params
 	    .groupBy(_.asset)
 	    .filter{case(asset, _) => currencies contains asset}
   	    .map{ case (asset, p) => (asset, p.groupBy(_.instrument))} 
  	    .filter{ case (_, instruments) => (instruments contains ndsKey) && (instruments contains fxKey)}
  	    .mapValues(_.mapValues(_.map(r => {
  	      if (r.maturity == null || r.maturity.trim.isEmpty) (null, r.value)
  	      else (new qlPeriod(r.maturity.trim), r.value)
  	    }).toMap))
  	    
  	  
  	  nonemptyinstruments.map{ case (ccy, values) => 
  		  val ndscurve = NDSCurve(valuedate, ccy, values(ndsKey)).orNull
  		  val fxvalue = values(fxKey).head._2
  		  NDSDiscountCurve(ndscurve, pivotDiscount, pivotTenorBS, fxvalue)
  	  	}.toSet
  	}
  
  
} 

