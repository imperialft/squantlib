package squantlib.test

import scala.collection.immutable.{TreeMap, SortedMap}
import scala.collection.Iterable

import squantlib.test.sample.CurveSamples
import squantlib.parameter.yieldparameter._
import squantlib.model.discountcurve._

import org.jquantlib.time.{ Date => JDate, Period => JPeriod, TimeUnit, Frequency}
import org.jquantlib.indexes.ibor._
import org.jquantlib.indexes._
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.{USDCurrency, BRLCurrency}
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.currencies.Europe.EURCurrency

import org.junit._
import org.junit.Assert._

class DiscountCurveTest {
	
	  val vd = new JDate(10, 5, 2012)
	  val curves = new CurveSamples(vd)
	  val ratecurves = curves.ratecurves
	  val fxcurves = curves.fxcurves
	  
	  val JPY_curvemodel = ratecurves('JPYcurve)
	  val USD_curvemodel = ratecurves('USDcurve)
	  val EUR_curvemodel = ratecurves('EURcurve)
	  val BRL_curvemodel = fxcurves('BRLcurve)
	  
	  val period6m = new JPeriod(6, TimeUnit.Months)
	  val period30y = new JPeriod(30, TimeUnit.Years)
	      
	  /**
	   * test spread for each currency
	   */
	  val spreads = {
	  	  val spread1 = new FlatVector(vd, Map(period6m -> 0.00))
		  val spread2 = new FlatVector(vd, Map(period6m -> 0.02))
		  val spread3 = new FlatVector(vd, Map(period6m -> -0.01))
	  	  List(spread1, spread2, spread3)
	  }
	  
	  val JPY_ZC = spreads.map(s => JPY_curvemodel.getZC(s))
	  val USD_ZC = spreads.map(s => USD_curvemodel.getZC(s))
	  val EUR_ZC = spreads.map(s => EUR_curvemodel.getZC(s))
	  
	  /**
	   * Cross currency discounting
	   */
	  val JPY_ccyZC = USD_ZC.map(s => JPY_curvemodel.getZC(USD_curvemodel, s))
	  val USD_ccyZC = EUR_ZC.map(s => USD_curvemodel.getZC(EUR_curvemodel, s))
	  val JPY_EURccyZC = USD_ccyZC.map(s => JPY_curvemodel.getZC(USD_curvemodel, s))
	  val BRL_ZC = USD_ZC.map(s => BRL_curvemodel.getZC(USD_curvemodel, s))
	  
	    /**
	   * Discount test unit - discount by same currency
	   */
	  @Test def testZC():Unit = {
	    
	    val maxerror = 0.000001
        val currencymodels = List((JPY_curvemodel, JPY_ZC), (USD_curvemodel, USD_ZC), (EUR_curvemodel, EUR_ZC))
		val maturities = (12 to 120 by 12).toList
		
		println("Discount factors")
		println("[ccy, maturity, rate, period, dcf, value]")
        currencymodels foreach { models => val ccy = models._1; val zcs = models._2;
        	for (i <- 0 to spreads.length - 1) {val zc = zcs(i); 
        	  maturities foreach {m =>
        	    val period = ccy.swap.fixperiod
        	    val maturity = new JPeriod(m, TimeUnit.Months)
        	    val spread36 = (if(ccy.swap.floatindex.tenor.length >= 6) ccy.tenorbasis.value(maturity) else 0.0)
        	    val spreaddisc = spreads(i).value(maturity)
        	    val spread = spreaddisc - spread36
        	    val rate = ccy.swap.value(maturity) + spread * ccy.swap.floatindex.dayCounter.annualDayCount / ccy.swap.fixdaycount.annualDayCount
	    		val dcf = ccy.swap.fixdaycount
	    		val zcval = fixedleg(ccy, zc, m, period.tenor, rate, dcf, true)
	    		println(ccy.currency.code + ", " + maturity + ", " + rate + ", " + period + ", " + dcf + " => " + zcval)
	    		assert(zcval - 1.00 < maxerror)
        	  }
        	}
        }
	  }
	  
	    /**
	   * Display the ZC curve content
	   */
	  @Test def display():Unit = {
	    displaycurve
	  }
	  
	    /**
	   * Discount test unit - discount by cross currency
	   */
	  @Test def testZCccy():Unit = {
	    
	    val maxerror = 0.0005
        val currencymodels = List((JPY_curvemodel, JPY_ccyZC, USD_curvemodel, USD_ZC))
		val maturities = (12 to 120 by 12).toList
		
		println("Cross currency discount")
		println("[ccy, maturity, rate, period, dcf, value]")
        currencymodels foreach { models => val ccy = models._1; val ccy2 = models._3; val zcs = models._2; val zcs2 = models._4
        	for (i <- 0 to spreads.length - 1) {val zc = zcs(i); val zc2 = zcs2(i) 
        	  maturities foreach {m =>
        	    val maturity = new JPeriod(m, TimeUnit.Months)

	    		val dcfdom = ccy.swap.fixdaycount
        	    val perioddom = ccy.swap.fixperiod
        	    val basis36 = (if(ccy.swap.floatindex.tenor.length >= 6) ccy.tenorbasis.value(maturity) else 0.0)
        	    val basis = ccy.basis.value(maturity)
        	    val spreaddom = - basis36 + basis
        	    val ratedom = ccy.swap.value(maturity) + spreaddom * ccy.swap.floatindex.dayCounter.annualDayCount / ccy.swap.fixdaycount.annualDayCount
        	    
	    		val dcffor = ccy2.swap.floatindex.dayCounter
        	    val periodfor = ccy2.swap.floatindex.tenor.length
        	    val discountspd = spreads(i).value(maturity)
        	    val spreadfor = discountspd
        	    val ratefor = spreadfor
        	      
	    		val pvdom = fixedleg(ccy, zc, m, perioddom.tenor, ratedom, dcfdom, true)
	    		val pvfor = fixedleg(ccy2, zc2, m, periodfor, ratefor, dcffor, false)
	    		
	    		println(ccy.currency.code + ", " + maturity + ", " + ratedom+ ", " + perioddom + ", " + dcfdom + " => " + pvdom)
	    		println(ccy2.currency.code + ", " + maturity + ", " + ratefor+ ", " + periodfor + ", " + dcffor + " => " + pvfor)
	    		assert(math.abs(pvdom + pvfor - 1) < maxerror)
        	  }
        	}
        }
	  }
	  
	  
	  def fixedleg(curve:RateCurve, discount:DiscountCurve, maturity:Int, period:Int, rate:Double, dcf:DayCounter, finalpayment:Boolean) : Double = {
		  val dates = (for (i <- 0 to maturity by period) yield (i, vd.add(new JPeriod(i, TimeUnit.Months)))) toMap
		  val swaptotal = (for (i <- dates.keySet if i < maturity) yield {
		    rate * dcf.yearFraction(dates(i), dates(i+period)) * discount.zc.value(dates(i+period))
		    }).sum + (if (finalpayment) discount.zc.value(dates(maturity)) else 0.0)
		  swaptotal
	  }

	  /**
	   * View
	   */
	  @Test def displaycurve() : Unit = {
	    
          curves.displaycurves
	    
		  /**
		   * Result display parameters. Max maturity = testperiod * testcase months
		   */
		  val testperiod = 12 // every X months
		  val testcase = 30 // number of outputs 
		  var inputset = for (i <- 0 to (testcase * testperiod) if i % testperiod == 0) yield new JPeriod(i, TimeUnit.Months)
		  def valuelist(xlist:Seq[String]):String =  xlist.length match { case 0 => ""; case 1 => xlist(0); case 2 => xlist(0) + ", " + xlist(1); case _ => xlist.head + ", " + valuelist(xlist.tail)}
		  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
		  val percent = (x:Double, decimals:Int) => (rounding(x*100, decimals)) + "%"
	    
	    
  		  println("** JPY discounted by JPY **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(JPY_ZC.map(z => rounding(z.zc.value(d), 4).toString))) })
		  
		  println("** USD discounted by USD **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(USD_ZC.map(z => rounding(z.zc.value(d), 4).toString))) })
		  
		  println("** EUR discounted by EUR**")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(EUR_ZC.map(z => rounding(z.zc.value(d), 4).toString))) })
	

		  /**
		   * Cross currency discounting - curve discounted by pivotcurve
		   */
		  
		  println("** JPY discounted by USD **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(JPY_ccyZC.map(z => rounding(z.zc.value(d), 4).toString))) })
	
		  /**
		   * Cross pivot curve discounted by non-pivot
		   */
		  
		  println("** USD discounted by EUR **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(USD_ccyZC.map(z => rounding(z.zc.value(d), 4).toString))) })
		  
		  /**
		   * Non-pivot discounted by non-pivot (through pivot)
		   */
		  println("** JPY discounted by EUR **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(JPY_EURccyZC.map(z => rounding(z.zc.value(d), 4).toString))) })
		  
		  /**
		   * FX discounting by pivotcurve
		   */
		  
		  println("** BRL discounted by USD **") 
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + valuelist(BRL_ZC.map(z => rounding(z.zc.value(d), 4).toString))) })	    
	  }

	  
}

