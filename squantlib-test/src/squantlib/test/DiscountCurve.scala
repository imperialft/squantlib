package squantlib.test

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import squantlib.parameter._
import squantlib.ratecurve._

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.Frequency
import org.jquantlib.indexes.ibor._
import org.jquantlib.indexes._
import org.jquantlib.daycounters._
import org.jquantlib.currencies.America._

object DiscountCurve {
	
	def main(args:Array[String]) : Unit = {
	  
	  /**
	   * Setting up view functions
	   */
	  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
	  val percent = (x:Double, decimals:Int) => (rounding(x*100, decimals)) + "%"
	  val vdescribe = (v : TimeVector) => { "value " + v.valuedate.shortDate.toString + ":" + percent(v.value(v.valuedate), 2) + " to " + v.maxdate.shortDate.toString + ":" + percent(v.value(v.maxdays), 2) }
	  val cashdescribe = (r : CashCurve) => r.currency.code + " " + r.floatindex.dayCounter
	  val swapdescribe = (r : SwapCurve) => r.currency.code + " " + r.fixperiod.tenor + "m " + r.fixdaycount.name + " vs " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name
	  val basisdescribe = (r : BasisSwapCurve) => r.currency.code + " " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name + " vs " + r.pivotfloatindex.currency.code + " " + r.pivotfloatindex.tenor.length + "m " + r.pivotfloatindex.dayCounter.name
	  val basis36describe = (r : TenorBasisSwapCurve) => r.currency.code + " " + r.shortindex.tenor.length + "m " + r.shortindex.dayCounter.name + " vs " + " " + r.longindex.tenor.length + "m " + r.longindex.dayCounter.name
	  
	  
	  /**
	   * Value date and a "random" period (we use flat curve)
	   */
	  val vd = new JDate(5, 6, 2012)
	  val period6m = new JPeriod(6, TimeUnit.Months)
	  val period30y = new JPeriod(30, TimeUnit.Years)

	  
	  /**
	   * Result display parameters. Max maturity = testperiod * testcase months
	   */
	  val testperiod = 6 // every X months
	  val testcase = 30*2 // number of outputs 
	  var inputset = for (i <- 0 to (testcase * testperiod) if i % testperiod == 0) yield new JPeriod(i, TimeUnit.Months)
	  
	  /**
	   * test spread for each currency
	   */
  	  val spread1 = new FlatVector(vd, Map(period6m -> 0.00))
	  val spread2 = new FlatVector(vd, Map(period6m -> 0.02))
	  val spread3 = new FlatVector(vd, Map(period6m -> -0.01))
	   
	  
	  /**
	   * JPY curve definition
	   */
	  val JPY_cashinput = Map(period6m -> 0.01)
	  val JPY_cash_curve = new FlatVector(vd, JPY_cashinput)
	  val JPY_cash_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
 	  val JPY_cash = new CashCurve(JPY_cash_curve, JPY_cash_floatindex)

	  val JPY_swapinput = Map(period30y -> 0.01)
	  val JPY_swap_curve = new FlatVector(vd, JPY_swapinput)
	  val JPY_swap_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
	  val JPY_swap_fixdaycount = new Actual365Fixed
	  val JPY_swap_fixperiod = Frequency.Semiannual
	  val JPY_swap = new SwapCurve(JPY_swap_curve, JPY_swap_floatindex, JPY_swap_fixdaycount, JPY_swap_fixperiod)
	  
	  val JPY_basisinput = Map(period30y -> -0.005)
	  val JPY_basis_curve = new FlatVector(vd, JPY_basisinput)
	  val JPY_basis_floatindex = new JPYLibor(new JPeriod(3, TimeUnit.Months))
 	  val JPY_basis = new BasisSwapCurve(JPY_basis_curve, JPY_basis_floatindex)
	  
	  val JPY_basis36input = Map(period30y -> 0.002)
	  val JPY_basis36_curve = new FlatVector(vd, JPY_basis36input)
	  val JPY_basis36_sindex = new JPYLibor(new JPeriod(3, TimeUnit.Months))
	  val JPY_basis36_lindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
 	  val JPY_basis36 = new TenorBasisSwapCurve(JPY_basis36_curve, JPY_basis36_sindex, JPY_basis36_lindex)
	  
	  println("** JPY Curve **")
	  println("Cash: " + cashdescribe(JPY_cash) + " " + vdescribe(JPY_cash.rate))
	  println("Swap: "  + swapdescribe(JPY_swap) + " " + vdescribe(JPY_swap.rate))
	  println("BSccy: " + basisdescribe(JPY_basis) + " " + vdescribe(JPY_basis.rate))
	  println("BS3m6m: " + basis36describe(JPY_basis36) + " " + vdescribe(JPY_basis36.rate))
	  
	  val JPY_curvemodel = new LiborDiscountCurve(JPY_cash, JPY_swap, JPY_basis, JPY_basis36, vd)
	  val JPY_ZC1 = JPY_curvemodel.getZC(spread1)
	  val JPY_ZC2 = JPY_curvemodel.getZC(spread2)
	  val JPY_ZC3 = JPY_curvemodel.getZC(spread3)
	  
	  println("** JPY Discount Curve **")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(JPY_ZC1.zc.value(d), 4) + ", " + rounding(JPY_ZC2.zc.value(d), 4) + ", " + rounding(JPY_ZC3.zc.value(d), 4) + " : " + rounding(JPY_ZC1.discountspread.value(d), 4) + ", " + rounding(JPY_ZC2.discountspread.value(d), 4) + ", " + rounding(JPY_ZC3.discountspread.value(d), 4))})
	  
	  /**
	   * USD curve definition
	   */
	  val USD_cashinput = Map(period6m -> 0.05)
	  val USD_cash_curve = new FlatVector(vd, USD_cashinput)
	  val USD_cash_floatindex = new USDLibor(new JPeriod(6, TimeUnit.Months))
 	  val USD_cash = new CashCurve(USD_cash_curve, USD_cash_floatindex)

	  val USD_swapinput = Map(period30y -> 0.05)
	  val USD_swap_curve = new FlatVector(vd, USD_swapinput)
	  val USD_swap_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
	  val USD_swap_fixdaycount = new Actual360
	  val USD_swap_fixperiod = Frequency.Annual
	  val USD_swap = new SwapCurve(USD_swap_curve, USD_swap_floatindex, USD_swap_fixdaycount, USD_swap_fixperiod)
	  
	  val USD_basisinput = Map(period30y -> 0.00)
	  val USD_basis_curve = new FlatVector(vd, USD_basisinput)
	  val USD_basis_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
 	  val USD_basis = new BasisSwapCurve(USD_basis_curve, USD_basis_floatindex)
	  
	  val USD_basis36input = Map(period30y -> 0.00)
	  val USD_basis36_curve = new FlatVector(vd, USD_basis36input)
	  val USD_basis36_sindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
	  val USD_basis36_lindex = new USDLibor(new JPeriod(6, TimeUnit.Months))
 	  val USD_basis36 = new TenorBasisSwapCurve(USD_basis36_curve, USD_basis36_sindex, USD_basis36_lindex)
	  	  
	  println("**USD**")
	  println("Cash: " + cashdescribe(USD_cash) + " " + vdescribe(USD_cash.rate))
	  println("Swap: "  + swapdescribe(USD_swap) + " " + vdescribe(USD_swap.rate))
	  println("BSccy: " + basisdescribe(USD_basis) + " " + vdescribe(USD_basis.rate))
	  println("BS3m6m: " + basis36describe(USD_basis36) + " " + vdescribe(USD_basis36.rate))
	  
	  val USD_curvemodel = new LiborDiscountCurve(USD_cash, USD_swap, USD_basis, USD_basis36, vd)
	  val USD_ZC1 = USD_curvemodel.getZC(spread1)
	  val USD_ZC2 = USD_curvemodel.getZC(spread2)
	  val USD_ZC3 = USD_curvemodel.getZC(spread3)
	  
	  println("USD Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(USD_ZC1.zc.value(d), 4) + ", " + rounding(USD_ZC2.zc.value(d), 4) + ", " + rounding(USD_ZC3.zc.value(d), 4) + " : " + rounding(USD_ZC1.discountspread.value(d), 4) + ", " + rounding(USD_ZC2.discountspread.value(d), 4) + ", " + rounding(USD_ZC3.discountspread.value(d), 4))})
	  
	  
	  /**
	   * EUR curve definition
	   */
	  val EUR_cashinput = Map(period6m -> 0.03)
	  val EUR_cash_curve = new FlatVector(vd, EUR_cashinput)
	  val EUR_cash_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
 	  val EUR_cash = new CashCurve(EUR_cash_curve, EUR_cash_floatindex)

	  val EUR_swapinput = Map(period30y -> 0.03)
	  val EUR_swap_curve = new FlatVector(vd, EUR_swapinput)
	  val EUR_swap_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
	  val EUR_swap_fixdaycount = new Thirty360
	  val EUR_swap_fixperiod = Frequency.Annual
	  val EUR_swap = new SwapCurve(EUR_swap_curve, EUR_swap_floatindex, EUR_swap_fixdaycount, EUR_swap_fixperiod)
	  
	  val EUR_basisinput = Map(period30y -> -0.005)
	  val EUR_basis_curve = new FlatVector(vd, EUR_basisinput)
	  val EUR_basis_floatindex = new Euribor(new JPeriod(3, TimeUnit.Months))
 	  val EUR_basis = new BasisSwapCurve(EUR_basis_curve, EUR_basis_floatindex)
	  
	  val EUR_basis36input = Map(period30y -> 0.001)
	  val EUR_basis36_curve = new FlatVector(vd, EUR_basis36input)
	  val EUR_basis36_sindex = new Euribor(new JPeriod(3, TimeUnit.Months))
	  val EUR_basis36_lindex = new Euribor(new JPeriod(6, TimeUnit.Months))
 	  val EUR_basis36 = new TenorBasisSwapCurve(EUR_basis36_curve, EUR_basis36_sindex, EUR_basis36_lindex)
	  	  
	  println("**EUR**")
	  println("Cash: " + cashdescribe(EUR_cash) + " " + vdescribe(EUR_cash.rate))
	  println("Swap: "  + swapdescribe(EUR_swap) + " " + vdescribe(EUR_swap.rate))
	  println("BSccy: " + basisdescribe(EUR_basis) + " " + vdescribe(EUR_basis.rate))
	  println("BS3m6m: " + basis36describe(EUR_basis36) + " " + vdescribe(EUR_basis36.rate))
	  
	  val EUR_curvemodel = new LiborDiscountCurve(EUR_cash, EUR_swap, EUR_basis, EUR_basis36, vd)
	  val EUR_ZC1 = EUR_curvemodel.getZC(spread1)
	  val EUR_ZC2 = EUR_curvemodel.getZC(spread2)
	  val EUR_ZC3 = EUR_curvemodel.getZC(spread3)
	  
	  println("EUR Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(EUR_ZC1.zc.value(d), 4) + ", " + rounding(EUR_ZC2.zc.value(d), 4) + ", " + rounding(EUR_ZC3.zc.value(d), 4) + " : " + rounding(EUR_ZC1.discountspread.value(d), 4) + ", " + rounding(EUR_ZC2.discountspread.value(d), 4) + ", " + rounding(EUR_ZC3.discountspread.value(d), 4))})
	  

	  /**
	   * BRL curve definition
	   */
	  var BRL_fx = 1.58
	  
	  var BRL_points = TreeMap.empty[JPeriod, Double]
	  BRL_points ++= Map(new JPeriod(6, TimeUnit.Months) -> 301d)
	  BRL_points ++= Map(new JPeriod(9, TimeUnit.Months) -> 994d)
	  BRL_points ++= Map(new JPeriod(12, TimeUnit.Months) -> 1354d)
	  BRL_points ++= Map(new JPeriod(12*2, TimeUnit.Months) -> 2489d)
	  BRL_points ++= Map(new JPeriod(12*3, TimeUnit.Months) -> 4243d)
	  BRL_points ++= Map(new JPeriod(12*5, TimeUnit.Months) -> 6554d)
	  BRL_points ++= Map(new JPeriod(12*7, TimeUnit.Months) -> 9117d)
	  BRL_points ++= Map(new JPeriod(12*10, TimeUnit.Months) -> 13317d)
	  
	  val BRL_pointscurve = new SplineNoExtrapolation(vd, BRL_points, 1)
	  val BRL_multiplier = 10000
	  val BRL_currency = new BRLCurrency
 	  val BRL_pivotcurrency = new USDCurrency
	  val BRL_swappt = new SwapPointCurve(BRL_pointscurve, BRL_multiplier, BRL_currency, BRL_pivotcurrency)
	  
	  println("**BRL**")
	  println("fx: " + BRL_fx)
	  println("points: "  + vdescribe(BRL_pointscurve))
	  
	  val BRL_curvemodel = new FXDiscountCurve(BRL_swappt, BRL_fx, vd)
	  
	  	  
	  /**
	   * Cross currency discounting - curve discounted by pivotcurve
	   */
	  
	  println("** JPY discounted by USD **")
	  val JPY_ccyZC1 = JPY_curvemodel.getZC(USD_curvemodel, USD_ZC1)
	  val JPY_ccyZC2 = JPY_curvemodel.getZC(USD_curvemodel, USD_ZC2)
	  val JPY_ccyZC3 = JPY_curvemodel.getZC(USD_curvemodel, USD_ZC3)
	  
	  println("Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(JPY_ccyZC1.zc.value(d), 4) + ", " + rounding(JPY_ccyZC2.zc.value(d), 4) + ", " + rounding(JPY_ccyZC3.zc.value(d), 4) + " : " + rounding(JPY_ccyZC1.discountspread.value(d), 4) + ", " + rounding(JPY_ccyZC2.discountspread.value(d), 4) + ", " + rounding(JPY_ccyZC3.discountspread.value(d), 4))})


	  /**
	   * Cross pivot curve discounted by non-pivot
	   */
	  
	  println("** USD discounted by EUR **")
	  val USD_ccyZC1 = USD_curvemodel.getZC(EUR_curvemodel, EUR_ZC1)
	  val USD_ccyZC2 = USD_curvemodel.getZC(EUR_curvemodel, EUR_ZC2)
	  val USD_ccyZC3 = USD_curvemodel.getZC(EUR_curvemodel, EUR_ZC3)
	  
	  println("Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(USD_ccyZC1.zc.value(d), 4) + ", " + rounding(USD_ccyZC2.zc.value(d), 4) + ", " + rounding(USD_ccyZC3.zc.value(d), 4) + " : " + rounding(USD_ccyZC1.discountspread.value(d), 4) + ", " + rounding(USD_ccyZC2.discountspread.value(d), 4) + ", " + rounding(USD_ccyZC3.discountspread.value(d), 4))})

	  
	  /**
	   * Non-pivot discounted by non-pivot (through pivot)
	   */
	  println("** JPY discounted by EUR **")
	  val JPY_EURccyZC1 = JPY_curvemodel.getZC(USD_curvemodel, USD_ccyZC1)
	  val JPY_EURccyZC2 = JPY_curvemodel.getZC(USD_curvemodel, USD_ccyZC2)
	  val JPY_EURccyZC3 = JPY_curvemodel.getZC(USD_curvemodel, USD_ccyZC3)
	  
	  println("Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(JPY_EURccyZC1.zc.value(d), 4) + ", " + rounding(JPY_EURccyZC2.zc.value(d), 4) + ", " + rounding(JPY_EURccyZC3.zc.value(d), 4) + " : " + rounding(JPY_EURccyZC1.discountspread.value(d), 4) + ", " + rounding(JPY_EURccyZC2.discountspread.value(d), 4) + ", " + rounding(JPY_EURccyZC3.discountspread.value(d), 4))})
	  
	  
	  /**
	   * FX discounting by pivotcurve
	   */
	  
	  println("** BRL discounted by USD **")
	  val BRL_ccyZC1 = BRL_curvemodel.getZC(USD_curvemodel, USD_ZC1)
	  val BRL_ccyZC2 = BRL_curvemodel.getZC(USD_curvemodel, USD_ZC2)
	  val BRL_ccyZC3 = BRL_curvemodel.getZC(USD_curvemodel, USD_ZC3)
	  
	  println("Discount Curve")
	  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(BRL_ccyZC1.zc.value(d), 4) + ", " + rounding(BRL_ccyZC2.zc.value(d), 4) + ", " + rounding(BRL_ccyZC3.zc.value(d), 4) + " : " + "N/A" + ", " + "N/A" + ", " + "N/A")})
	  
    }
}

