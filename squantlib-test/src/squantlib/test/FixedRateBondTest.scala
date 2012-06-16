package squantlib.test

import scala.collection.JavaConversions._

import squantlib.termstructures.ZCImpliedYieldTermStructure

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

import org.junit._
import org.junit.Assert._

class FixedRateBondTest {
	
	val valuedate = new JDate(8, 3, 2012)

	val bond = {
		val issuedate = new JDate(7, 3, 2010)
		val maturity = new JDate(7, 3, 2020)
		val currency = new Asia.JPYCurrency
		val settlementdays = 0
		val faceamount = 100.0
		val coupons:Array[Double] = (for(i <- 0 to 9) yield 0.05) toArray
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
	   * display the curve contents and discount factor
	   */
	@Test def checkcontents : Unit = {
		println("bond information")
		println("notional:" + bond.notional(valuedate))
		println("value date:" + valuedate.shortDate.toString)
		println("issue date:" + bond.issueDate)
		println("maturity date:" + bond.maturityDate)
		println("frequency:" + bond.frequency.name)
		println("calendar:" + bond.calendar.name)
		println("next coupon:" + bond.nextCoupon(valuedate))
		println("accrued:" + bond.accruedAmount(valuedate))
		println("redemption:" + bond.redemption.amount() + " vd " + bond.redemption.date.shortDate.toString)
		
		println("cashflow schedule:")
		val cashflowlegs = bond.cashflows.iterator
		while (cashflowlegs.hasNext) { val c = cashflowlegs.next; println(c.date.shortDate.toString + " - " + c.amount) }
	}
	
	
	/**
	   * display the curve contents and discount factor
	   */
	@Test def checkyields : Unit = {
		
		val cashflowlegs = bond.cashflows.iterator
		val accrued = bond.accruedAmount(valuedate)
		val cashflows = cashflowlegs.toList.filter(_.date.gt(valuedate)).map(m => (m.date, m.amount))
		
		println("cashflow")
		println("accrued:" + accrued)
		cashflows foreach { c => println(c._1.shortDate.toString + " " + c._2)}
		
		val accuracy = 0.0001
		val price = List(50, 80, 100, 120, 180)
		
		val daycount = new Thirty360
		def pricefromyield(cashflow:List[(JDate, Double)], rate:Double, discountf:((Double, JDate) => Double)):Double = cashflow.map(c => c._2 * discountf(rate, c._1)).sum
		
		println("Continuous yields")
		val continuousyields = price.map(p => (p -> bond.`yield`(p, daycount, Compounding.Continuous, Frequency.NoFrequency, valuedate, accuracy, 1000))) toMap
		val continuousf = (rate:Double, date:JDate) => math.exp(-rate * daycount.yearFraction(valuedate, date))
		val continuousprice = continuousyields.map(y => (y._2, pricefromyield(cashflows, y._2, continuousf) - accrued))
		price foreach { p => val yld = continuousyields(p); val price = continuousprice(yld); val error = math.abs(p - price);
						println(p + " => " + yld + " => " + price + " error " + error);
						assert(error < accuracy)}
		
		println("Annual yields")
		val annualyields = price.map(p => (p -> bond.`yield`(p, daycount, Compounding.Compounded, Frequency.Annual, valuedate, accuracy, 1000))) toMap
		val annualf = (rate:Double, date:JDate) => 1 / math.pow(1.0 + rate, daycount.yearFraction(valuedate, date))
		val annualprice = annualyields.map(y => (y._2, pricefromyield(cashflows, y._2, annualf) - accrued))
		price foreach { p => val yld = annualyields(p); val price = annualprice(yld); val error = math.abs(p - price);
						println(p + " => " + yld + " => " + price + " error " + error);
						assert(error < accuracy)}
		
		println("Semiannual yields")
		val semiannyields = price.map(p => (p -> bond.`yield`(p, daycount, Compounding.Compounded, Frequency.Semiannual, valuedate, accuracy, 1000))) toMap
		val semiannf = (rate:Double, date:JDate) => 1 / math.pow(1.0 + rate/2, daycount.yearFraction(valuedate, date) * 2)
		val semiannprice = semiannyields.map(y => (y._2, pricefromyield(cashflows, y._2, semiannf) - accrued))
		price foreach { p => val yld = semiannyields(p); val price = semiannprice(yld); val error = math.abs(p - price);
						println(p + " => " + yld + " => " + price + " error " + error);
						assert(error < accuracy)}

		println("Simple yields")
		val nocompyields = price.map(p => (p -> bond.`yield`(p, daycount, Compounding.None, Frequency.Annual, valuedate, accuracy))) toMap
		val fullcashflow = cashflows.map(c => c._2).sum
		println("total cashflow:" + fullcashflow)
		val nocompprice = nocompyields.map(y => (y._2, (fullcashflow - accrued) / (1 + y._2 * daycount.yearFraction(valuedate, bond.maturityDate))))
		price foreach { p => val yld = nocompyields(p); val price = nocompprice(yld); val error = math.abs(p - price);
						println(p + " => " + yld + " => " + price + " error " + error);
						assert(error < accuracy)
						}
		
//		val simpleyields = price.map(p => (p -> bond.`yield`(p, daycount, Compounding.Simple, Frequency.Annual, valuedate, accuracy))) toMap
	}
	
}