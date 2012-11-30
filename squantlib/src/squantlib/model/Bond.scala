package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, _}
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.payoff.{Payoffs, Schedule}
import squantlib.setting.initializer.{DayAdjustments, Currencies}
import org.jquantlib.daycounters.Thirty360

/**
 * Bond class with enclosed risk analysis functions.
 */
class Bond(val db:dbBond)  {
  
	val defaultAdjustment = BusinessDayConvention.ModifiedFollowing
	val defaultDayCounter = new Thirty360
	
	val issueDate = new qlDate(db.issuedate)
	
	val maturity = new qlDate(db.maturity)
	
	val calendar = db.calendar
	
	val period = db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}
	
	val calendarConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
	
	val paymentConvention = DayAdjustments.getOrElse(db.payment_adj, defaultAdjustment)
	
	val maturityConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
	
	val rule = DateGeneration.Rule.Backward
	
	val fixingInArrears = db.inarrears.isDefined && db.inarrears == 0
	
	val noticeDay = db.cpnnotice.getOrElse(0)
	
	val firstDate = None
	
	val nextToLastdate = None
	  
	val schedule = if (period.isEmpty) None
		else Some(new Schedule(issueDate, maturity, period.get, calendar, calendarConvention,
	      paymentConvention, maturityConvention, rule, fixingInArrears, noticeDay, firstDate, nextToLastdate))
	
	val coupon = Payoffs(db.coupon)
	
	val isValidCoupon = schedule.isDefined && (coupon.size == 1 || coupon.size == schedule.size)
	
	val currency = Currencies(db.currencyid)
	
} 
	
