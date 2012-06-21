package squantlib.test.sample

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import squantlib.parameter.yieldparameter._
import squantlib.model.discountcurve._

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.Frequency
import org.jquantlib.indexes.ibor._
import org.jquantlib.indexes._
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.currencies.Europe.EURCurrency
import org.jquantlib.currencies.America.BRLCurrency

import org.junit._
import org.junit.Assert._

class CurveSamples(val vd:JDate) {
	  /**
	   * random period (we use flat curve)
	   */
	  val period6m = new JPeriod(6, TimeUnit.Months)
	  val period30y = new JPeriod(30, TimeUnit.Years)
	  
	  /**
	   * JPY curve definition
	   */
	  
	  val JPY_curvemodel = {
	    
	    val JPY_cash = {
			  val JPY_cashinput = Map(period6m -> 0.01)
			  val JPY_cash_curve = new FlatVector(vd, JPY_cashinput)
			  val JPY_cash_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
			  new CashCurve(JPY_cash_curve, JPY_cash_floatindex)
		  }
		  
		  val JPY_swap = {
			  val JPY_swapinput = Map(period30y -> 0.01)
			  val JPY_swap_curve = new FlatVector(vd, JPY_swapinput)
			  val JPY_swap_floatindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
			  val JPY_swap_fixdaycount = new Actual365Fixed
			  val JPY_swap_fixperiod = Frequency.Semiannual
			  new SwapCurve(JPY_swap_curve, JPY_swap_floatindex, JPY_swap_fixdaycount, JPY_swap_fixperiod)
		  }
		  
		  val JPY_basis = {
			  val JPY_basisinput = Map(period30y -> -0.005)
			  val JPY_basis_curve = new FlatVector(vd, JPY_basisinput)
			  val JPY_basis_floatindex = new JPYLibor(new JPeriod(3, TimeUnit.Months))
		 	  new BasisSwapCurve(JPY_basis_curve, JPY_basis_floatindex)
		  }
		  
		  val JPY_basis36 = {
			  val JPY_basis36input = Map(period30y -> 0.002)
			  val JPY_basis36_curve = new FlatVector(vd, JPY_basis36input)
			  val JPY_basis36_sindex = new JPYLibor(new JPeriod(3, TimeUnit.Months))
			  val JPY_basis36_lindex = new JPYLibor(new JPeriod(6, TimeUnit.Months))
		 	  new TenorBasisSwapCurve(JPY_basis36_curve, JPY_basis36_sindex, JPY_basis36_lindex)
		  }
		  
		  new LiborDiscountCurve(JPY_cash, JPY_swap, JPY_basis, JPY_basis36, vd)
	  }
	  
	  
	  /**
	   * USD curve definition
	   */
	  
	  val USD_curvemodel = {
	    
		  val USD_cash = {
			  val USD_cashinput = Map(period6m -> 0.05)
			  val USD_cash_curve = new FlatVector(vd, USD_cashinput)
			  val USD_cash_floatindex = new USDLibor(new JPeriod(6, TimeUnit.Months))
		 	  new CashCurve(USD_cash_curve, USD_cash_floatindex)
		  }
	
		  val USD_swap = {
			  val USD_swapinput = Map(period30y -> 0.05)
			  val USD_swap_curve = new FlatVector(vd, USD_swapinput)
			  val USD_swap_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
			  val USD_swap_fixdaycount = new Actual360
			  val USD_swap_fixperiod = Frequency.Annual
			  new SwapCurve(USD_swap_curve, USD_swap_floatindex, USD_swap_fixdaycount, USD_swap_fixperiod)
		  }
		  
		  val USD_basis = {
			  val USD_basisinput = Map(period30y -> 0.00)
			  val USD_basis_curve = new FlatVector(vd, USD_basisinput)
			  val USD_basis_floatindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
		 	  new BasisSwapCurve(USD_basis_curve, USD_basis_floatindex)
		  }
		  
		  val USD_basis36 = {
			  val USD_basis36input = Map(period30y -> 0.00)
			  val USD_basis36_curve = new FlatVector(vd, USD_basis36input)
			  val USD_basis36_sindex = new USDLibor(new JPeriod(3, TimeUnit.Months))
			  val USD_basis36_lindex = new USDLibor(new JPeriod(6, TimeUnit.Months))
		 	  new TenorBasisSwapCurve(USD_basis36_curve, USD_basis36_sindex, USD_basis36_lindex)
		  }
		  
		  new LiborDiscountCurve(USD_cash, USD_swap, USD_basis, USD_basis36, vd)
	  }
	  
	  /**
	   * EUR curve definition
	   */
	  
	  val EUR_curvemodel = {
	    
		  val EUR_cash = {
			  val EUR_cashinput = Map(period6m -> 0.03)
			  val EUR_cash_curve = new FlatVector(vd, EUR_cashinput)
			  val EUR_cash_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
		 	  new CashCurve(EUR_cash_curve, EUR_cash_floatindex)
		  }
	
		  val EUR_swap = {
			  val EUR_swapinput = Map(period30y -> 0.03)
			  val EUR_swap_curve = new FlatVector(vd, EUR_swapinput)
			  val EUR_swap_floatindex = new Euribor(new JPeriod(6, TimeUnit.Months))
			  val EUR_swap_fixdaycount = new Thirty360
			  val EUR_swap_fixperiod = Frequency.Annual
			  new SwapCurve(EUR_swap_curve, EUR_swap_floatindex, EUR_swap_fixdaycount, EUR_swap_fixperiod)
		  }
		  
		  val EUR_basis = {
			  val EUR_basisinput = Map(period30y -> -0.005)
			  val EUR_basis_curve = new FlatVector(vd, EUR_basisinput)
			  val EUR_basis_floatindex = new Euribor(new JPeriod(3, TimeUnit.Months))
		 	  new BasisSwapCurve(EUR_basis_curve, EUR_basis_floatindex)
		  }
		  
		  val EUR_basis36 = {
			  val EUR_basis36input = Map(period30y -> 0.001)
			  val EUR_basis36_curve = new FlatVector(vd, EUR_basis36input)
			  val EUR_basis36_sindex = new Euribor(new JPeriod(3, TimeUnit.Months))
			  val EUR_basis36_lindex = new Euribor(new JPeriod(6, TimeUnit.Months))
		 	  new TenorBasisSwapCurve(EUR_basis36_curve, EUR_basis36_sindex, EUR_basis36_lindex)
		  }
		  
		  new LiborDiscountCurve(EUR_cash, EUR_swap, EUR_basis, EUR_basis36, vd)
	  }
	  
	
	  /**
	   * BRL curve definition
	   */
	  
	  val BRL_curvemodel = {
	    
		  var BRL_fx = 1.58
		  
		  val BRL_points = {
			  var points = TreeMap.empty[JPeriod, Double]
			  points ++= Map(new JPeriod(6, TimeUnit.Months) -> 301d)
			  points ++= Map(new JPeriod(9, TimeUnit.Months) -> 994d)
			  points ++= Map(new JPeriod(12, TimeUnit.Months) -> 1354d)
			  points ++= Map(new JPeriod(12*2, TimeUnit.Months) -> 2489d)
			  points ++= Map(new JPeriod(12*3, TimeUnit.Months) -> 4243d)
			  points ++= Map(new JPeriod(12*5, TimeUnit.Months) -> 6554d)
			  points ++= Map(new JPeriod(12*7, TimeUnit.Months) -> 9117d)
			  points ++= Map(new JPeriod(12*10, TimeUnit.Months) -> 13317d)
			  points
		  }
		   
		  val BRL_pointscurve = new SplineNoExtrapolation(vd, BRL_points, 1)
		  val BRL_multiplier = 10000
		  val BRL_currency = new BRLCurrency
	 	  val BRL_pivotcurrency = new USDCurrency
		  val BRL_swappt = new SwapPointCurve(BRL_pointscurve, BRL_multiplier, BRL_currency, BRL_pivotcurrency)
		  
		  new FXDiscountCurve(BRL_swappt, BRL_fx, vd)
	  }
	  
	  /**
	   * Export 
	   */
	  def ratecurves:Map[Symbol, RateCurve] = Map(
	      ('JPYcurve -> JPY_curvemodel), 
	      ('EURcurve -> EUR_curvemodel), 
	      ('USDcurve -> USD_curvemodel)) 
	      
	  def fxcurves:Map[Symbol, FXCurve] = Map(
	      ('BRLcurve -> BRL_curvemodel))

	  def discountablecurves:Map[Symbol, DiscountableCurve] = ratecurves ++ fxcurves
	  
	    /**
	   * View
	   */
	  def displaycurves = discountablecurves.foreach(x => x._2.describecontent)
	  
}