//package squantlib.ratecurve
//
//import scala.collection.immutable.TreeMap
//
//import squantlib.parameter._
//
//import org.jquantlib.time.{ Date => JDate }
//import org.jquantlib.time.{ Period => JPeriod }
//import org.jquantlib.time.TimeUnit
//import org.jquantlib.daycounters.DayCounter;
// 
//import org.apache.commons.math3.analysis.function.Log
//import org.apache.commons.math3.analysis.function.Exp
//
//
//class LiborDiscountCurve (val cash : CashCurve, val swap:SwapCurve, val basis:BasisSwapCurve, val tenorbasis:TenorBasisSwapCurve, val valuedate : JDate) 
//extends RateCurve{
//  require (List(cash.valuedate, swap.valuedate, basis.valuedate, tenorbasis.valuedate).forall(_ == valuedate))
//  
//	def getZC(spread : TimeVector) : DiscountCurve = {
//	  /**
//	   * initialize empty containers
//	   */
//	  var ZC : TreeMap[JPeriod, Double] = TreeMap.empty
//	  var ZCspread : TreeMap[JPeriod, Double] = TreeMap.empty
//
//	  /**
//	   * initialize day count fractions
//	   */
//	  val dcffloat = swap.floatIndex.dayCounter().annualDayCount()
//	  val dcffix = swap.fixDaycount.annualDayCount()
//	  val fixperiod = 12 / swap.fixPeriod.toInteger()
//	  var localduration = 0.0
//	  
//	  /**
//	   * initialize maturities and daycounts.
//	   * Daycount is for swap fixed leg convention. (not to be used for cash rate)
//	   */
//	  val maxmaturity = (JPeriod.months(swap.rate.maxPeriod, valuedate)).toInt
//	  val periods = (for (m <- (List(0, 3, 6, 9) ++ (12 until maxmaturity by fixperiod))) yield (m -> (new JPeriod(m, TimeUnit.Months)))) toMap
//	  val maturities = (for (m <- periods.keySet) yield (m -> valuedate.add(periods(m)))) toMap
//	  val dcf = (for (m <- fixperiod until maxmaturity by fixperiod) yield (m -> swap.fixDaycount.yearFraction(maturities(m-fixperiod), maturities(m)))) toMap
//	  
//	  /**
//	   * spot zero coupon = 1.00
//	   */
//	  ZC ++= Map(periods(0) -> 1.00)
//	  
//	  /**
//	   * using cash rate to compute zero coupon < 12 months.
//	   * note: we are assuming 3m-9m basis spread = 3m-6m basis spread
//	   */
//	  val cashrange = for (m <- periods.keySet if m < 12; if m > 0) yield m
//	  ZCspread ++= (for (m <- cashrange; p = periods(m)) yield (p, spread.value(p)))
//	  ZC ++= (for (m <- cashrange; p = periods(m); bs3m6m = if (m == 3) 0.0 else tenorbasis.value(p); zcspd = ZCspread(p)) 
//		  		yield (p, 1 / (1 + (cash.value(p) + zcspd - bs3m6m) * dcffloat * m / 12)))
//	  localduration += (for(m <- cashrange if m % fixperiod == 0; p = periods(m)) yield (ZC(p) * dcf(m))).sum
//	  
//	  /**
//	   * using swap rate to compute zero coupon >= 1year 
//	   * note: we are assuming no basis below 3 months and above 6 months
//	   */
//	  val swaprange = (for (m <- periods.keySet if m >= 12) yield m)
//	  ZCspread ++= (for (m <- swaprange; p = periods(m)) yield (p, spread.value(p)))
//	  
//	  
////	  for (m <- periods.keySet if m >= 12) {
////	    val period = periods(m)
////	    val bs3m6m = if (swap.floatIndex.tenor().length() <= 3) 0.0 else tenorbasis.value(period)
////	    val zcspd = spread.value(period)
////        val realRate = swap.value(period) + (zcspd - bs3m6m) * dcffloat / dcffix
////        val fixduration = localduration
////        val zcXm = (1 - realRate * fixduration) / (1 + realRate * dcf(m))
////        ZCspread ++= Map(period -> zcspd)
////        ZC ++= Map(period -> zcXm)
////        localduration += zcXm * dcf(m)
////	  }
//	  
//	  /**
//	   * Construct new discount curve object.
//	   * ZC vector is spline interpolation with exponential extrapolation
//	   * ZCspread vector is spline interpolation with no extrapolation and with 2 additional points
//	   */
//	  val ZCvector = new SplineEExtrapolation(valuedate, ZC)
//	  val ZCspdvector = new SplineNoExtrapolation(valuedate, ZCspread, 2)
//	  new DiscountCurve(ZCvector, ZCspdvector)
//  }
//  
//}