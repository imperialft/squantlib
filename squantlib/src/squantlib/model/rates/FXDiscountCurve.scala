package squantlib.model.rates

import scala.collection.SortedMap
import scala.collection.immutable.{TreeMap, SortedSet}
import squantlib.model.yieldparameter.{YieldParameter, SplineEExtrapolation, FlatVector}
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.Thirty360
import squantlib.database.schemadefinitions.RateFXParameter
import squantlib.setting.RateConvention
//import squantlib.setting.initializer.RateConventions


case class FXDiscountCurve(swappoint:SwapPointCurve, fx:Double) extends FXCurve{

	  val currency = swappoint.currency
	  val pivotcurrency = swappoint.pivotcurrency
	  val valuedate = swappoint.valuedate
	  
	  /** 
	   * Builds zero coupon curve using the curve itself as discount currency 
	   * - Not available for FX curve as risk-free rate is defined only in terms of another currency.
	   */
	  def getZC(spread : YieldParameter) : DiscountCurve = {
	    println("Cannot discount FX-defined curve without reference to pivot currency")
	    return null
	  }

	  /** 
	   * Builds zero coupon curve using external curve as discount currency.
	   * Discounting curve must be pivot currency (usually USD)
	   */
	  def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {
	    require(refincurve != null && refinZC != null && refincurve.currency == swappoint.pivotcurrency)
	    
		  /**
		   * day count initialization
		   */
		  val maxmaturity = qlPeriod.months(swappoint.points.maxperiod, valuedate).toInt
		  val zcfreq = 3
		  val zcmonths:Seq[Int] = (for (m <- 0 to maxmaturity if m % zcfreq == 0) yield m).sorted
		  val zcperiods = TreeMap(zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))) : _*) 
		  val swapptperiods = zcperiods.filter(p => p._1 > 0)
	    
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
		   * initialize refinancing zc
		   */
		  val refinZCvector = swapptperiods.map{case (m, p) => (m, refinZC.zc(p))}
		  
		  /**
		   * initialize forward fx
		   */
		  val fwdfxvector = swapptperiods.map{case (m, p) => (m, swappoint.value(p, fx))}
		  
		  
		  /**
		   * compute zero coupon
		   */
		  swapptperiods foreach { m => val fwdfx = fwdfxvector(m._1); val zc = refinZCvector(m._1); val p = m._2; ZC ++= Map(p -> zc * fx/fwdfx) }
		  
		  /**
		   * Construct new discount curve object.
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = SplineEExtrapolation(valuedate, ZC, 1)
		  new DiscountCurve(currency, ZCvector, fx)
	    
	  }
	  
	  def shiftRate(shift:(Double, Double) => Double):FXDiscountCurve = new FXDiscountCurve(swappoint.shifted(shift), fx)
	  def multFX(mult:Double):FXDiscountCurve= new FXDiscountCurve(swappoint, fx * mult)
  
}

object FXDiscountCurve {
  
	val swappointKey = "SwapPt"
	val fxKey = "FX"
	  
	/**
	 * Constructs LiborDiscountCurve from InputParameter per each combination of currency & paramset.
	 * Invalid input parameter sets are ignored.
	 * @param set of InputParameter
	 * @returns map from (Currency, ParamSet) to LiborDiscountCurve
	 */
  	def apply(params:Set[RateFXParameter], valuedate:qlDate):Set[FXDiscountCurve] = {
    
	  val currencies = RateConvention.toMap.filter{case (k, v) => v.useFXdiscount }.keySet
	  
  	  val nonemptyinstruments:Map[String, Map[String, Map[qlPeriod, Double]]] = 
 	    params
 	    .groupBy(_.asset)
 	    .filter{case(asset, _) => currencies contains asset}
   	    .map{ case (asset, p) => (asset, p.groupBy(_.instrument))} 
  	    .filter{ case (_, instruments) => (instruments contains swappointKey) && (instruments contains fxKey)}
  	    .mapValues(_.mapValues(_.map(r => {
  	      if (r.maturity == null || r.maturity.trim.isEmpty) (null, r.value)
  	      else (new qlPeriod(r.maturity.trim), r.value)
  	    }).toMap))
  	  
  	  nonemptyinstruments.map{ case (ccy, values) => 
  		  val swapptcurve = SwapPointCurve(valuedate, ccy, values(swappointKey)).orNull
  		  FXDiscountCurve(swapptcurve, values(fxKey).head._2)
  	  	}.toSet
  	}
  
} 

