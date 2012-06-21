package squantlib.model.discountcurve

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedSet

import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.parameter.yieldparameter.SplineEExtrapolation

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.daycounters.Thirty360


class FXDiscountCurve(val swappoint:SwapPointCurve, val fx:Double, val valuedate:JDate) extends FXCurve{

	  val currency = swappoint.currency
	  val pivotcurrency = swappoint.pivotcurrency
	  val basedaycount = new Thirty360
	  
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
		  new DiscountCurve(ZCvector)
	    
	  }
  
}