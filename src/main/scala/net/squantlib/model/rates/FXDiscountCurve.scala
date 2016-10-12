package net.squantlib.model.rates

import net.squantlib.util.Date
import net.squantlib.util.DisplayUtils._
import net.squantlib.model.yieldparameter.{YieldParameter, SplineEExtrapolation, FlatVector}
import org.jquantlib.time.{Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.Thirty360
import net.squantlib.model.rates.convention.RateConvention
import scala.annotation.tailrec
import scala.collection.breakOut

case class FXDiscountCurve(swappoint:SwapPointCurve, fx:Double) extends FXCurve{
  
	val currency = swappoint.currency
	val pivotcurrency = swappoint.pivotcurrency
	val valuedate = swappoint.valuedate
	  
	/**
	 * day count initialization
	 */
	val maxmaturity = valuedate.months(swappoint.points.maxperiod).toInt
	val zcfreq = 3
	val zcmonths = (for (m <- 0 to maxmaturity if m % zcfreq == 0) yield m).sorted
	val zcperiods:Map[Int, qlPeriod] = zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))) (breakOut)
	val sortedperiods = zcperiods.toList.sortBy(_._1)
	val swapptperiods = zcperiods.filter(_._1 > 0)
	  
	/** 
	 * Builds zero coupon curve using the curve itself as discount currency 
	 * - Not available for FX curve as risk-free rate is defined only in terms of another currency.
	 */
	def getZC(spread : YieldParameter) : DiscountCurve = {
	  errorOutput("Cannot discount FX-defined curve without reference to pivot currency")
	  return null
	}

	/** 
	 * Builds zero coupon curve using external curve as discount currency.
	 * Discounting curve must be pivot currency (usually USD)
	 */
	def getZC(refincurve:RateCurve, refinZC:DiscountCurve) : DiscountCurve = {
	  require(refincurve != null && refinZC != null && refincurve.currency == swappoint.pivotcurrency)
	  
	 val refinZCvector:Map[Int, Double] = swapptperiods.mapValues(refinZC.zc(_))
	 val fwdfxvector = swapptperiods.mapValues(swappoint.value(_, fx))
		  
	  @tailrec def zcRec(dates:List[(Int, qlPeriod)], zc:List[Double]):List[Double] = {
	    if (dates.isEmpty) zc.reverse
	    else dates.head match {
	      case (m, p) if m == 0 => zcRec(dates.tail, 1.00 :: zc)
	      case (m, p) => zcRec(dates.tail, (refinZCvector(m) * fx / fwdfxvector(m)) :: zc)
	    }
	  }
	    
	  val zcvalues = zcRec(sortedperiods, List.empty)
	  val zc:Map[qlPeriod, Double] = (sortedperiods.unzip._2 zip zcvalues) (breakOut)
	  val ZCvector = SplineEExtrapolation(valuedate, zc, 1)
	  
	  DiscountCurve(currency, ZCvector, fx)
	}
	  
	override def shiftRate(shift:(Double, Double) => Double):FXDiscountCurve = FXDiscountCurve(swappoint.shifted(shift), fx)

	override def multFX(mult:Double):FXDiscountCurve= FXDiscountCurve(swappoint, fx * mult)
  
}

