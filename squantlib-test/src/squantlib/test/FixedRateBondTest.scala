package squantlib.test

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

object FixedRateBondTest {
	
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
	   * Main function will display the curve contents and discount factor
	   */
	def main(args:Array[String]) : Unit = {
	    val valuedate = new JDate(10, 3, 2010)
		println("bond intialised")
		println("notional:" + bond.notional)
		println("value date:" + valuedate.shortDate.toString)
		println("issue date:" + bond.issueDate)
		println("maturity date:" + bond.maturityDate)
		println("frequency:" + bond.frequency.name)
		println("calendar:" + bond.calendar.name)
		println("next coupon:" + bond.nextCoupon)
		println("accrued:" + bond.accruedAmount(valuedate))
		println("redemption:" + bond.redemption.amount() + " vd " + bond.redemption.date.shortDate.toString)
		
		println("cashflow schedule:")
		val cashflowlegs = bond.cashflows.iterator
		while (cashflowlegs.hasNext) { val c = cashflowlegs.next; println(c.date.shortDate.toString + " - " + c.amount) }
		
		val accuracy = 0.0001
		val price = List(50, 80, 100, 120, 180)
		val nocompyields = price.map(p => (p -> bond.`yield`(p, new Thirty360, Compounding.None, Frequency.Annual, valuedate, accuracy))) toMap
		val simpleyields = price.map(p => (p -> bond.`yield`(p, new Thirty360, Compounding.Simple, Frequency.Annual, valuedate, accuracy))) toMap
		val continuousyields = price.map(p => (p -> bond.`yield`(p, new Thirty360, Compounding.Continuous, Frequency.Annual, valuedate, accuracy, 1000))) toMap
		val annualyields = price.map(p => (p -> bond.`yield`(p, new Thirty360, Compounding.Compounded, Frequency.Annual, valuedate, accuracy, 1000))) toMap
		val sayields = price.map(p => (p -> bond.`yield`(p, new Thirty360, Compounding.Compounded, Frequency.Semiannual, valuedate, accuracy, 1000))) toMap

		println("yields")
		println("[price, none, simple, annual compounding, SA compounding, continuous]")
		price foreach { y => 
		    val none = nocompyields(y)
		    val simple = simpleyields(y)
		    val annual = annualyields(y)
		    val semian = sayields(y)
		    val cont = continuousyields(y)
		    println(y + ", " + none + ", " + simple + ", " + annual + ", " + semian + ", " + cont)
			}
		
	}
}