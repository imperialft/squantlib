package squantlib.model.discountcurve

import scala.collection.SortedMap
import scala.collection.immutable.{TreeMap, SortedSet}
import squantlib.parameter.yieldparameter.{YieldParameter, SplineEExtrapolation}
import org.jquantlib.time.{ Date => JDate, Period => JPeriod, TimeUnit}
import org.jquantlib.daycounters.Thirty360
import squantlib.database.schemadefinitions.RateFXParameter
import squantlib.initializer.RateConvention

class FXDiscountCurve(val swappoint:SwapPointCurve, val fx:Double) extends FXCurve{

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
		  val maxmaturity = JPeriod.months(swappoint.points.maxperiod, valuedate).toInt
		  val zcfreq = 3
		  val zcmonths:Seq[Int] = (for (m <- 0 to maxmaturity if m % zcfreq == 0) yield m).sorted
		  val zcperiods = TreeMap(zcmonths.map(m => (m, new JPeriod(m, TimeUnit.Months))) : _*) 
		  val swapptperiods = zcperiods.filter(p => p._1 > 0)
	    
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
		   * initialize refinancing zc
		   */
		  val refinZCvector = swapptperiods.map(p => (p._1, refinZC.zc.value(p._2)))
		  
		  /**
		   * initialize forward fx
		   */
		  val fwdfxvector = swapptperiods.map(p => (p._1, swappoint.value(p._2, fx)))
		  
		  
		  /**
		   * compute zero coupon
		   */
		  swapptperiods foreach { m => val fwdfx = fwdfxvector(m._1); val zc = refinZCvector(m._1); val p = m._2; ZC ++= Map(p -> zc * fx/fwdfx) }
		  
		  /**
		   * Construct new discount curve object.
		   * ZC vector is spline interpolation with exponential extrapolation
		   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
		   */
		  val ZCvector = new SplineEExtrapolation(valuedate, ZC, 1)
		  new DiscountCurve(currency, ZCvector, fx)
	    
	  }
  
}

object FXDiscountCurve {
  import squantlib.parameter.yieldparameter.FlatVector
  
	val swappointKey = "SwapPt"
	val fxKey = "FX"
	val pivotccy = "USD"
	  
	/**
	 * Constructs LiborDiscountCurve from InputParameter per each combination of currency & paramset.
	 * Invalid input parameter sets are ignored.
	 * @param set of InputParameter
	 * @returns map from (Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getcurves(params:Traversable[RateFXParameter]):Iterable[FXDiscountCurve] = {
	  val conventions:Map[String, RateConvention] = RateConvention.allConventions.filter{case (k, v) => v.useFXdiscount}
  	  val dateassetgroup = params.groupBy(p => p.asset).filter{case(k, v) => conventions.keySet.contains(k)}
  	  val instrumentgroup = dateassetgroup.map{ case (k, v) => (k, v.groupBy(p => p.instrument))} 
  	  val nonemptyinstruments = instrumentgroup.filter{ case (k, v) => (v.keySet.contains(swappointKey) && v.keySet.contains(fxKey))}
  	  
  	  nonemptyinstruments.map{ case (k, v) => {
  		  val conv = conventions(k)
  		  val valuedate = new JDate(v(swappointKey).head.paramdate)
  		  def toSortedMap(k:String) = SortedMap(v(k).toSeq.map(p => (new JPeriod(p.maturity), p.value)) :_*)
  		  val swapptcurve = conv.swappoint_constructor(valuedate, toSortedMap(swappointKey))
  		  new FXDiscountCurve(swapptcurve, v(fxKey).head.value)
  	  	}
  	  }
  	}
//  	  
//	/**
//	 * Constructs LiborDiscountCurve from InputParameter per each currency, only with specified paramset.
//	 * Invalid input paramter sets or unmatched paramset ID are ignored.
//	 * @param set of InputParameter
//	 * @returns map from Currency to LiborDiscountCurve
//	 */
//  	def getcurves(params:Traversable[InputParameter], paramset:String):Map[String, FXDiscountCurve] = {
//      val validparams = params.filter(p => p.paramset == paramset)
//  	  getcurves(validparams) map { case ((k1, k2), v) => (k1, v) }
//  	}
  	
} 

