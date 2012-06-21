package squantlib.test.sample

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

import org.junit._
import org.junit.Assert._

object BondSamples {
  
    val couponrate = 0.05
	val bondmaturity = new JDate(7, 3, 2020)
	val bondJPY = {
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
		  val convention = BusinessDayConvention.Unadjusted
		  val maturityconvention = BusinessDayConvention.Unadjusted
		  val rule = DateGeneration.Rule.Backward
		  val endofmonth = false
		  new Schedule(effectivedate, terminationdate, tenor, calendar, convention, maturityconvention, rule, endofmonth)
		}
		
		new FixedRateBond(settlementdays, faceamount, schedule, coupons, accrualdaycounter, paymentconvention, redemption, issuedate)
	}
    

}