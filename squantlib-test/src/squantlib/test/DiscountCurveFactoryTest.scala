package squantlib.test

import squantlib.model.discountcurve.DiscountableCurve
import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.test.sample.CurveSamples

import org.jquantlib.time.TimeUnit
import org.jquantlib.time.{Date => JDate}
import org.jquantlib.time.{Period => JPeriod}
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.currencies.America.BRLCurrency
import org.jquantlib.currencies.Europe.EURCurrency
import org.jquantlib.currencies.Currency

import org.junit._
import org.junit.Assert._

class DiscountCurveFactoryTest {
  
	  val vd = new JDate(10, 5, 2012)
	  val curves = new CurveSamples(vd)
	  val ratecurves = curves.ratecurves
	  val fxcurves = curves.fxcurves
	  val JPY_curvemodel = ratecurves('JPYcurve)
	  val USD_curvemodel = ratecurves('USDcurve)
	  val EUR_curvemodel = ratecurves('EURcurve)
	  val BRL_curvemodel = fxcurves('BRLcurve)
  
  	    /**
	   * Discount curve factory check
	   */
	  @Test def discountcurvefactory():Unit = {
		  val currencylist:Map[String, DiscountableCurve] = Map(
		      ((new JPYCurrency).code, JPY_curvemodel),
		      ((new USDCurrency).code, USD_curvemodel),
		      ((new EURCurrency).code, EUR_curvemodel),
		      ((new BRLCurrency).code, BRL_curvemodel))
		      
		  val factory = new DiscountCurveFactory(currencylist)

		  /**
		   * Result display parameters. Max maturity = testperiod * testcase months
		   */
		  val testperiod = 12 // every X months
		  val testcase = 30 // number of outputs 
		  var inputset = for (i <- 0 to (testcase * testperiod) if i % testperiod == 0) yield new JPeriod(i, TimeUnit.Months)
		  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
		  def valuelist(xlist:Seq[String]):String =  xlist.length match { case 0 => ""; case 1 => xlist(0); case 2 => xlist(0) + ", " + xlist(1); case _ => xlist.head + ", " + valuelist(xlist.tail)}
		  
		  println("** Factory Test **")
		  
		  var starttime = java.util.Calendar.getInstance.getTimeInMillis
		  val zc1 = factory.getdiscountcurve((new JPYCurrency).code, -0.01)
		  println("** JPY Discount by JPY Curve **")
		  println("[ZC1, ZC2, ZC3, spread1, spread2, spread3]")
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(zc1.zc.value(d), 4).toString) })
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")

		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  println("** JPY Discount by USD Curve **")
		  val zc2 = factory.getdiscountcurve((new JPYCurrency).code, (new USDCurrency).code, 0.01)
		  inputset.foreach( (d:JPeriod) => { println(d.toString() + ", " + rounding(zc2.zc.value(d), 4).toString) })
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")

		  println("** JPY Discount by EUR Curve**")
		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  val zc3 = factory.getdiscountcurve((new JPYCurrency).code, (new EURCurrency).code, 0.02)
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")
		  
		  println("** BRL Discount by EUR Curve**")
		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  val zcbrl1 = factory.getdiscountcurve((new BRLCurrency).code, (new EURCurrency).code, 0.02)
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")
		  
		  println("** BRL Discount by JPY Curve**")
		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  val zcbrl2 = factory.getdiscountcurve((new BRLCurrency).code, (new JPYCurrency).code, 0.02)
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")

		  println("** JPY Discount by EUR Curve x 10 (precalculated)**")
		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  for (i <- 1 to 10) {
			  val zc4 = factory.getdiscountcurve((new JPYCurrency).code, (new EURCurrency).code, 0.02)
		  }
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")
		  
		  println("** JPY Discount by EUR Curve x 10(new curves)**")
		  starttime = java.util.Calendar.getInstance.getTimeInMillis
		  for (i <- 1 to 10) {
			  val zc4 = factory.getdiscountcurve((new JPYCurrency).code, (new EURCurrency).code, i * 0.05)
		  }
		  println("Time elapsed: " + (java.util.Calendar.getInstance.getTimeInMillis - starttime) + " ms")
		  
		  
		  println("factory contents:")
		  factory.repository.keySet.foreach(k => println(k._1 + " " + k._2 + factory.repository(k).keySet.map(x => x)))
	  }
}