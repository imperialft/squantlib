package squantlib.test

import scala.collection.immutable.TreeSet

import squantlib.termstructures.ZCImpliedYieldTermStructure
import squantlib.parameter.yieldparameter._
import squantlib.model.discountcurve.{RateCurve, DiscountCurve}
import squantlib.test.sample.{BondSamples, CurveSamples}

import org.jquantlib.time.{Calendar, Date => JDate, Period => JPeriod, TimeUnit, Frequency}
import org.jquantlib.time.calendars._
import org.jquantlib.daycounters._
import org.jquantlib.termstructures.Compounding
import org.jquantlib.indexes.ibor._
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.cashflow.CashFlows
import org.jquantlib.currencies.Asia.JPYCurrency

import org.junit._
import org.junit.Assert._


class FixedRateBondPricingTest {
     
	  val valuedate = new JDate(7, 5, 2012)
	  val bond = BondSamples.bondJPY
	  val factory = (new CurveSamples(valuedate)).curvefactory
	  
	  def bondschedule(valuedate:JDate, maturity:JDate, period:JPeriod, calendar:Calendar, cleanprice:Boolean, adjusted:Boolean) : List[(JDate, JDate)] = {
		var datelist : List[(JDate, JDate)] = List.empty
		var currentdate = maturity
		println("maturity:" + maturity.shortDate.toString)
		
		while (currentdate.gt(valuedate))
		{
		  val enddate = if (adjusted) calendar.adjust(currentdate) else currentdate
		  currentdate = currentdate.sub(period)
		  
		  val startdate = if (!cleanprice || currentdate.gt(valuedate)) {
				  				if (adjusted) calendar.adjust(currentdate) 
				  				else currentdate}
			  			  else valuedate
			  			  
		  datelist ::= new Pair(startdate, enddate)
		  println(startdate.shortDate.toString + " - " + enddate.shortDate.toString)
		}
		datelist 
	  }

	  def bondprice(curve:RateCurve, discount:DiscountCurve, maturity:JDate, dates:List[(JDate, JDate)], rate:Double, dcf:DayCounter, finalpayment:Boolean, calendar:Calendar) = {
	      val couponpv = dates.map(d => {
	    	  val daycount = dcf.yearFraction(d._1, d._2)
	    	  val zc = discount.zc.value(calendar.adjust(d._2))
	    	  rate * daycount * zc
	      }).sum
	      	
	      val redempv = if (finalpayment) discount.zc.value(maturity) else 0.0
	      couponpv + redempv
	  }
	  
	    /**
	   * Discount test unit - discount by same currency
	   */
	  @Test def fixedbondprice():Unit = {
		    /**
		     * test spread for each currency
		     */
		    val period6m = new JPeriod(6, TimeUnit.Months)
		    
		    val spreads = Set(0.0, 0.02, -0.01)
			val accuracy = 0.05
			val calendar = new Japan
			val settlement = 0
			val bondmaturity = new JDate(9, 3, 2020)
		    
			/**
			   * Compute bond analysis
			   */
		    val results = spreads.map{spd => new {
		    	val spread = spd
			    val zc = factory.getdiscountcurve((new JPYCurrency).code, spread).orNull
			    val curve = factory.curves((new JPYCurrency).code)
				val termstructure = new ZCImpliedYieldTermStructure(zc, calendar, settlement, valuedate)
				val bondengine = new DiscountingBondEngine(termstructure)
				bond.setPricingEngine(bondengine, valuedate)
				
				/**
				   * Dirty price test
				   */
				val (pricedirty, pricedirty_manual, errordirty) = {
		    		val modelprice = bond.dirtyPrice
					val couponrate = bond.nextCoupon(valuedate) 
					val schedule = bondschedule(valuedate, bondmaturity, bond.frequency.toPeriod, calendar, false, false)
					val ratecurve = curve match { case c:RateCurve => c; case _ => throw new ClassCastException }
					val manualprice = bondprice(ratecurve, zc, bond.maturityDate, schedule, couponrate, bond.dayCounter, true, calendar) * 100
					(modelprice, manualprice, math.abs(manualprice - modelprice))
				}
				assert(errordirty < accuracy)
				
				/**
				   * Clean price test
				   */
				val (priceclean, priceclean_manual, errorclean) = {
		    		val modelprice = bond.cleanPrice
					val couponrate = bond.nextCoupon(valuedate) 
					val schedule = bondschedule(valuedate, bondmaturity, bond.frequency.toPeriod, calendar, true, false)
					val ratecurve = curve match { case c:RateCurve => c; case _ => throw new ClassCastException }
					val manualprice = bondprice(ratecurve, zc, bond.maturityDate, schedule, couponrate, bond.dayCounter, true, calendar) * 100
					(modelprice, manualprice, math.abs(manualprice - modelprice))
				}
				assert(errorclean < accuracy)
				
				/**
				   * Risk analysis
				   */
				val cfmodel = CashFlows.getInstance
				val cashflows = bond.cashflows
				val bps = cfmodel.bps(cashflows, termstructure, valuedate)
				val atmrate = cfmodel.atmRate(cashflows, termstructure, valuedate, valuedate, 0, 0)

				val irr = cfmodel.irr(cashflows, pricedirty, new Thirty360, Compounding.Continuous, Frequency.NoFrequency, valuedate, 0.001, 1000, 0.01)
				val nextrate = cfmodel.nextCouponRate(cashflows, valuedate)
				val nextamount = cfmodel.nextCashFlow(cashflows, valuedate).amount
				
				val interestrate = termstructure.forwardRate(valuedate, bond.maturityDate, termstructure.dayCounter, Compounding.Compounded, Frequency.Semiannual)
				
				val simpleduration = cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Simple, valuedate)
				val modifiedduration = cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Modified, valuedate)
				val macauleyduration = cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Macaulay, valuedate)
				
				val yieldvaluebp = cfmodel.yieldValueBasisPoint(cashflows, interestrate, valuedate)
				val convexity = cfmodel.convexity(cashflows, interestrate, valuedate)
		    }}
			
		    
			/**
			   * Display results
			   */
		    val eol = sys.props("line.separator")
		    println("cashflow schedule:")
			val cashflowlegs = bond.cashflows.iterator
			
			while (cashflowlegs.hasNext) { 
			  val c = cashflowlegs.next
			  println(c.date.shortDate.toString + " - " + c.amount) 
			  }
		    
			println("Dirty Price")
			println("[spread, manual price, model price, error]")
		    println(results.map(p => p.spread + ", " + p.pricedirty + ", " + p.pricedirty_manual + ", " + p.errordirty).mkString(eol))
		    
			println("Clean Price")
			println("[spread, manual price, model price, error]")
		    println(results.map(p => p.spread + ", " + p.priceclean + ", " + p.priceclean_manual + ", " + p.errorclean).mkString(eol))
			
			println("Basis Point Values")
		    println(results.map(p => p.spread + " => " + p.bps).mkString(eol))

			println("ATM rate") 
		    println(results.map(p => p.spread + " => " + p.atmrate).mkString(eol))
			
			println("*** Initialise Interest rate ***") 
		    println(results.map(p => p.spread + " => " + p.interestrate.rate).mkString(eol))
			
			println("Duration")
			println("[simple, modified, maucauley]")
		    println(results.map(p => p.spread + ", " + p.simpleduration + ", " + p.modifiedduration + ", " + p.macauleyduration).mkString(eol))
		
			println("Yield Value Basis Point")
		    println(results.map(p => p.spread + " => " + p.yieldvaluebp).mkString(eol))
			
			println("Convexity")
		    println(results.map(p => p.spread + " => " + p.convexity).mkString(eol))
			
			println("IRR")
		    println(results.map(p => p.spread + " => " + p.irr).mkString(eol))
			
			println("Next Coupon Rate")
		    println(results.map(p => p.spread + " => " + p.nextrate).mkString(eol))

			println("Next Coupon Amount")
		    println(results.map(p => p.spread + " => " + p.nextamount).mkString(eol))
			
	  }

}

