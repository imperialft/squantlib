package squantlib.test

import scala.collection.immutable.TreeSet

import squantlib.termstructures.ZCImpliedYieldTermStructure
import squantlib.parameter._
import squantlib.ratecurve._

import org.jquantlib.time.Calendar
import org.jquantlib.time.Schedule
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.BusinessDayConvention
import org.jquantlib.currencies._
import org.jquantlib.time.DateGeneration
import org.jquantlib.time.Frequency
import org.jquantlib.time.calendars._
import org.jquantlib.daycounters._
import org.jquantlib.instruments.bonds.FixedRateBond
import org.jquantlib.termstructures.Compounding
import org.jquantlib.indexes.ibor._
import org.jquantlib.pricingengines.bond.DiscountingBondEngine

import org.junit._
import org.junit.Assert._


class FixedRateBondPricingTest {
     
	val vd = new JDate(7, 5, 2010)
    val couponrate = 0.05
	val bondmaturity = new JDate(7, 3, 2020)
	val bond = {
		val issuedate = new JDate(7, 3, 2010)
		val maturity = bondmaturity
		val currency = new Asia.JPYCurrency
		val settlementdays = 0
		val faceamount = 100.0
		val coupons:Array[Double] = (for(i <- 0 to 9) yield couponrate) toArray
		val accrualdaycounter = new Thirty360
		val paymentconvention = BusinessDayConvention.ModifiedFollowing
		val redemption = 100.0
		
		val schedule = {
		  val effectivedate = issuedate
		  val terminationdate = maturity
		  val tenor = new JPeriod(6, TimeUnit.Months)
		  val calendar = new Japan
		  val convention = BusinessDayConvention.ModifiedFollowing
		  val maturityconvention = BusinessDayConvention.ModifiedFollowing
		  val rule = DateGeneration.Rule.Backward
		  val endofmonth = false
		  new Schedule(effectivedate, terminationdate, tenor, calendar, convention, maturityconvention, rule, endofmonth)
		}
		
		new FixedRateBond(settlementdays, faceamount, schedule, coupons, accrualdaycounter, paymentconvention, redemption, issuedate)
	}
	
	
	  /**
	   * JPY curve definition
	   */
	
	  val period6m = new JPeriod(6, TimeUnit.Months)
	  val period30y = new JPeriod(30, TimeUnit.Years)
	
	  
	  /**
	   * test spread for each currency
	   */
	  val spreads = {
	  	  val spread1 = (0.00, new FlatVector(vd, Map(period6m -> 0.00)))
		  val spread2 = (0.02, new FlatVector(vd, Map(period6m -> 0.02)))
		  val spread3 = (-0.01, new FlatVector(vd, Map(period6m -> -0.01)))
	  	  List(spread1, spread2, spread3)
	  }
	   	  
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
	  val JPY_ZC = spreads.map(s => (s._1, JPY_curvemodel.getZC(s._2)))
	  
	  def bondschedule(valuedate:JDate, maturity:JDate, period:JPeriod, calendar:Calendar, cleanprice:Boolean) : List[(JDate, JDate)] = {
		var datelist : List[(JDate, JDate)] = List.empty
		var currentdate = maturity
		println("maturity:"+maturity.shortDate.toString)
		while (currentdate.gt(valuedate))
		{
		  val enddate = calendar.adjust(currentdate)
		  currentdate = currentdate.sub(period)
		  val startdate = if (!cleanprice || currentdate.gt(valuedate)) calendar.adjust(currentdate) else valuedate
//		  val startdate = calendar.adjust(currentdate)
		  datelist ::= new Pair(startdate, enddate)
		  println(startdate.shortDate.toString + " - " + enddate.shortDate.toString)
		}
		datelist
	  }

	  def bondprice(curve:RateCurve, discount:DiscountCurve, maturity:JDate, dates:List[(JDate, JDate)], rate:Double, dcf:DayCounter, finalpayment:Boolean) = 
	      dates.map(d => rate * dcf.yearFraction(d._1, d._2) * discount.zc.value(d._2)).sum  + (if (finalpayment) discount.zc.value(maturity) else 0.0)
	  
	    /**
	   * Discount test unit - discount by same currency
	   */
	  @Test def fixedbondprice():Unit = {
			val accuracy = 0.01
			println("cashflow schedule:")
			val cashflowlegs = bond.cashflows.iterator
			while (cashflowlegs.hasNext) { val c = cashflowlegs.next; println(c.date.shortDate.toString + " - " + c.amount) }
		    val calendar = new Japan
		    val settlement = 0
			val termstructs = JPY_ZC.map(zc => (zc._1, new ZCImpliedYieldTermStructure(JPY_curvemodel, zc._2, calendar, settlement, vd)))
			val bondengines = termstructs.map(ts => (ts._1, new DiscountingBondEngine(ts._2)))
			
		    /**
		   * Dirty price test
		   */
			val modelprice = bondengines.map(e => { bond.setPricingEngine(e._2, vd); (e._1, bond.dirtyPrice())} ) toMap
			val scheduledirty = bondschedule(vd, bondmaturity, bond.frequency.toPeriod, calendar, false)
			val manualprice = JPY_ZC.map( zc => (zc._1, bondprice(JPY_curvemodel, zc._2, bond.maturityDate, scheduledirty, couponrate, bond.dayCounter, true))) toMap
			
			println("[spread, manual price, model price, error]")
			manualprice foreach {
		    	p => { val price1 = manualprice(p._1) * 100; val price2 = modelprice(p._1); val error = math.abs(price1 - price2);
		    		   println(p._1 + ", " + price1 + ", " + price2 + ", " + error)
		    		   assert(error < accuracy)
		    	}
		    }
		    
		    /**
		   * Clean price test
		   */
			val modelpriceclean = bondengines.map(e => { bond.setPricingEngine(e._2, vd); (e._1, bond.cleanPrice())} ) toMap
			val scheduleclean = bondschedule(vd, bondmaturity, bond.frequency.toPeriod, calendar, true)
			val manualpriceclean = JPY_ZC.map( zc => (zc._1, bondprice(JPY_curvemodel, zc._2, bond.maturityDate, scheduleclean, couponrate, bond.dayCounter, true))) toMap
			
			println("[spread, manual price, model price, error]")
			manualprice foreach {
		    	p => { val price1 = manualpriceclean(p._1) * 100; val price2 = modelpriceclean(p._1); val error = math.abs(price1 - price2);
		    		   println(p._1 + ", " + price1 + ", " + price2 + ", " + error)
		    		   assert(error < accuracy)
		    	}
		    }
					    
			
	  }

}