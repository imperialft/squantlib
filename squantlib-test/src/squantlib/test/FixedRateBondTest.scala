//package squantlib.test
//
//import squantlib.termstructures.ZCImpliedYieldTermStructure
//
//import org.jquantlib.time.Schedule
//import org.jquantlib.time.{ Date => JDate }
//import org.jquantlib.time.{ Period => JPeriod }
//import org.jquantlib.time.TimeUnit
//import org.jquantlib.time.BusinessDayConvention
//import org.jquantlib.currencies._
//import org.jquantlib.time.DateGeneration
//import org.jquantlib.time.calendars._
//import org.jquantlib.daycounters._
//
//class FixedRateBondTest {
//	
//	val issuedate = new JDate(7, 3, 2010)
//	val maturity = new JDate(7, 3, 2020)
//	val currency = new Asia.JPYCurrency
//	
//	val settlementdays = 0
//	val faceamount = 100.0
//	
//	val schedule = {
//	  val effectivedate = issuedate
//	  val terminationdate = maturity
//	  val tenor = new JPeriod(6, TimeUnit.Months)
//	  val calendar = new Japan
//	  val convention = BusinessDayConvention.ModifiedFollowing
//	  val maturityconvention = BusinessDayConvention.ModifiedFollowing
//	  val rule = DateGeneration.Rule.Backward
//	  val endofmonth = false
//	  new Schedule(effectivedate, terminationdate, tenor, calendar, convention, maturityconvention, rule, endofmonth)
//	}
//  
//    public FixedRateBond(/*@Natural*/final int settlementDays,
//            /*@Real*/final double faceAmount,
//            final Schedule schedule,
//            final double[] coupons,
//            final DayCounter accrualDayCounter,
//            final BusinessDayConvention paymentConvention,
//            /*Real*/final double redemption,
//            final Date  issueDate){  
//}