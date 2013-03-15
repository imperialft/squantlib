package squantlib.model.rates

import squantlib.model.yieldparameter.{YieldParameter, SplineEExtrapolation, FlatVector}
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod, TimeUnit}
import org.jquantlib.daycounters.Thirty360
import squantlib.database.schemadefinitions.RateFXParameter
import squantlib.model.rates.convention.RateConvention
import scala.annotation.tailrec


case class FXDiscountCurve(swappoint:SwapPointCurve, fx:Double) extends FXCurve{
  
	val currency = swappoint.currency
	val pivotcurrency = swappoint.pivotcurrency
	val valuedate = swappoint.valuedate
	  
	/**
	 * day count initialization
	 */
	val maxmaturity = qlPeriod.months(swappoint.points.maxperiod, valuedate).toInt
	val zcfreq = 3
	val zcmonths = (for (m <- 0 to maxmaturity if m % zcfreq == 0) yield m).sorted
	val zcperiods = zcmonths.map(m => (m, new qlPeriod(m, TimeUnit.Months))).toMap
	val sortedperiods = zcperiods.toList.sortBy(_._1)
	val swapptperiods = zcperiods.filter(_._1 > 0)
	  
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
	  
	 val refinZCvector = swapptperiods.mapValues(refinZC.zc(_)).toMap
	 val fwdfxvector = swapptperiods.mapValues(swappoint.value(_, fx)).toMap
		  
	  @tailrec def zcRec(dates:List[(Int, qlPeriod)], zc:List[Double]):List[Double] = {
	    if (dates.isEmpty) zc
	    else dates.head match {
	      case (m, p) if m == 0 => zcRec(dates.tail, zc :+ 1.00)
	      case (m, p) => zcRec(dates.tail, zc :+ (refinZCvector(m) * fx / fwdfxvector(m)))
	    }
	  }
	    
	  val zcvalues = zcRec(sortedperiods, List.empty)
	  val zc:Map[qlPeriod, Double] = (sortedperiods.unzip._2 zip zcvalues).toMap
	  val ZCvector = SplineEExtrapolation(valuedate, zc, 1)
	  
	  DiscountCurve(currency, ZCvector, fx)
	}
	  
	override def shiftRate(shift:(Double, Double) => Double):FXDiscountCurve = FXDiscountCurve(swappoint.shifted(shift), fx)

	override def multFX(mult:Double):FXDiscountCurve= FXDiscountCurve(swappoint, fx * mult)
  
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

