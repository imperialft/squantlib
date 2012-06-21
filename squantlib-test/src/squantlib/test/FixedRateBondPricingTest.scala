package squantlib.test

import scala.collection.immutable.TreeSet

import squantlib.termstructures.ZCImpliedYieldTermStructure
import squantlib.parameter.yieldparameter._
import squantlib.model.discountcurve.RateCurve
import squantlib.model.discountcurve.DiscountCurve

import squantlib.test.sample.BondSamples
import squantlib.test.sample.CurveSamples

import org.jquantlib.time.Calendar
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.Frequency
import org.jquantlib.time.calendars._
import org.jquantlib.daycounters._
import org.jquantlib.termstructures.Compounding
import org.jquantlib.indexes.ibor._
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.cashflow.CashFlows

import org.junit._
import org.junit.Assert._


class FixedRateBondPricingTest {
     
	  val valuedate = new JDate(7, 5, 2012)
	  
	  val bond = BondSamples.bondJPY
	  val curves = (new CurveSamples(valuedate)).ratecurves
	  val JPY_curvemodel = curves('JPYcurve)
	  
	  /**
	   * test spread for each currency
	   */
	  val period6m = new JPeriod(6, TimeUnit.Months)
	  val spreads = {
	  	  val spread1 = (0.00, new FlatVector(valuedate, Map(period6m -> 0.00)))
		  val spread2 = (0.02, new FlatVector(valuedate, Map(period6m -> 0.02)))
		  val spread3 = (-0.01, new FlatVector(valuedate, Map(period6m -> -0.01)))
	  	  List(spread1, spread2, spread3)
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
			val termstructs = JPY_ZC.map(zc => (zc._1, new ZCImpliedYieldTermStructure(JPY_curvemodel, zc._2, calendar, settlement, valuedate)))
			val bondengines = termstructs.map(ts => (ts._1, new DiscountingBondEngine(ts._2)))
			val bondmaturity = bond.maturityDate
			val couponrate = bond.nextCoupon(valuedate) 
			
		    /**
		   * Dirty price test
		   */
			val modelprice = bondengines.map(e => { bond.setPricingEngine(e._2, valuedate); (e._1, bond.dirtyPrice())} ) toMap
			val scheduledirty = bondschedule(valuedate, bondmaturity, bond.frequency.toPeriod, calendar, false)
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
			val modelpriceclean = bondengines.map(e => { bond.setPricingEngine(e._2, valuedate); (e._1, bond.cleanPrice())} ) toMap
			val scheduleclean = bondschedule(valuedate, bondmaturity, bond.frequency.toPeriod, calendar, true)
			val manualpriceclean = JPY_ZC.map( zc => (zc._1, bondprice(JPY_curvemodel, zc._2, bond.maturityDate, scheduleclean, couponrate, bond.dayCounter, true))) toMap
			
			println("[spread, manual price, model price, error]")
			manualprice foreach {
		    	p => { val price1 = manualpriceclean(p._1) * 100; val price2 = modelpriceclean(p._1); val error = math.abs(price1 - price2);
		    		   println(p._1 + ", " + price1 + ", " + price2 + ", " + error)
		    		   assert(error < accuracy)
		    	}
		    }
			
			println("Basis Point Values")
			val cashflows = bond.cashflows
			val bps = termstructs.map(ts => (ts._1, CashFlows.getInstance.bps(cashflows, ts._2, valuedate)))
			bps.foreach(bp => println(bp._1 + " => " + bp._2))
			
			println("ATM rate") 
			val atmrate = termstructs.map(ts => (ts._1, CashFlows.getInstance.atmRate(cashflows, ts._2, valuedate, valuedate, 0, 0)))
			atmrate.foreach(r => println(r._1 + " => " + r._2))
			
			println("*** Initialise Interest rate ***") 
			val interestrates = termstructs.map(ts => (ts._1, ts._2.forwardRate(valuedate, bond.maturityDate, ts._2.dayCounter, Compounding.Compounded, Frequency.Semiannual)))
			interestrates.foreach(r => println(r._1 + " => " + r._2.rate))
			
			println("Duration")
			println("[simple, modified, maucauley]")
			val simpleduration = interestrates.map(r => (r._1, CashFlows.getInstance.duration(cashflows, r._2, CashFlows.Duration.Simple, valuedate))).toMap
			val modifiedduration = interestrates.map(r => (r._1, CashFlows.getInstance.duration(cashflows, r._2, CashFlows.Duration.Modified, valuedate))).toMap
			val macauleyduration = interestrates.map(r => (r._1, CashFlows.getInstance.duration(cashflows, r._2, CashFlows.Duration.Macaulay, valuedate))).toMap
			atmrate foreach {r => {
				val d1 = simpleduration(r._1); val d2 = modifiedduration(r._1);  val d3 = modifiedduration(r._1);  
				println(r._1 + " => " + d1 + ", " + d2 + ", " + d3 )}}
			
			println("Yield Value Basis Point")
			val yieldvaluebp = interestrates.map(r => (r._1, CashFlows.getInstance.yieldValueBasisPoint(cashflows, r._2, valuedate)))
			yieldvaluebp.foreach(r => println(r._1 + " => " + r._2))

			println("Convexity")
			val convexity = interestrates.map(r => (r._1, CashFlows.getInstance.convexity(cashflows, r._2, valuedate)))
			convexity.foreach(r => println(r._1 + " => " + r._2))
			
			
	  }

}