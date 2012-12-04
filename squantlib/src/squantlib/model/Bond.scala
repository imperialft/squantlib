package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, _}
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.payoff.{Payoffs, Schedule, Payoff, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.setting.initializer.{DayAdjustments, Currencies, Daycounters}
import org.jquantlib.daycounters.Thirty360
import org.codehaus.jackson.JsonNode
import squantlib.util.JsonUtils._

/**
 * Bond class with enclosed risk analysis functions.
 */
class Bond(
		val db:dbBond, 
	    val id:String,	
		val issueDate:qlDate,
		val maturity:qlDate,	
		val currency:Currency,
		val nominal:Double,	
		val denomination:Option[Double],	
		val period:qlPeriod,	
		val daycount:DayCounter,	
		val calendarAdjust:BusinessDayConvention,	
		val paymentAdjust:BusinessDayConvention,	
		val maturityAdjust:BusinessDayConvention,
		val calendar:Calendar,	
		val fixingInArrears:Boolean,	
		val couponNotice:Int,	
		val issuePrice:Option[Double],	
		val call:String,	
		val bondType:String,	
		val initialFX:Double,	
		val issuer:String,	
		val schedule:Schedule,	      
		val coupon:Payoffs,	
		val redemption:Payoff,	
		val settings:Option[JsonNode]) {
		
		val payoffs:Payoffs = (coupon :+ redemption).reorder(schedule.legorder)
		
		val payoffSchedule:List[(CalcPeriod, Payoff)] = schedule.zip(payoffs).toList
		
		def livePayoffSchedule(vd:qlDate):(Schedule, Payoffs) = {
	    	val livelegs = payoffSchedule.filter{case (cp, p) => (cp.paymentDate gt vd)}.unzip
	    	(Schedule(livelegs._1), Payoffs(livelegs._2))
		}
		
		
		val (eventDates, startDates, endDates, paymentDates) = 
		  (schedule.eventDates, schedule.startDates, schedule.endDates, schedule.paymentDates)
		
		val (eventYears, startYears, endYears, paymentYears) = 
		  (schedule.eventYears, schedule.startYears, schedule.endYears, schedule.paymentYears)
		  
		def discountCurve(market:CurveFactory):Option[DiscountCurve] = market.getDiscountCurve(currency, issuer)
		
		def discountFactors(market:CurveFactory):Option[List[Double]] = 
		  discountCurve(market).collect { case curve => paymentDates.map(curve(_))}
		
		val daycounts:List[Double] = schedule.map(_.daycount).toList
		
		def coefficients(market:CurveFactory):Option[List[Double]] = discountFactors(market) match {
		  case Some(l) => Some((for (i <- 0 to daycounts.size - 1) yield (l(i) * daycounts(i))).toList)
		  case None => None
		}
		
		override def toString:String = "ID: " + id + "\n" + 
		  "Schedule:\n" + payoffSchedule.map{case (s, po) => s.toString + " " + po.toString}.mkString("\n")

} 


object Bond {
  
	def apply(db:dbBond):Option[Bond] = {
	  
		val defaultDayCounter:DayCounter = new Thirty360
		val defaultAdjustment:BusinessDayConvention = BusinessDayConvention.ModifiedFollowing
		
		val id = db.id
		
		val issueDate:qlDate = new qlDate(db.issuedate)
		
		val maturity:qlDate = new qlDate(db.maturity)
		
		val nominal:Double = db.nominal
		
		val currency:Currency = Currencies(db.currencyid).orNull
		if (currency == null) { println(db.id  + " : currency not found"); return None}
		
		val denomination:Option[Double] = db.denomination
		
		val period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull
		if (period == null) { println(db.id  + " : period not defined"); return None}
		
		val daycount:DayCounter = Daycounters(db.daycount).getOrElse(defaultDayCounter)
		
		val calendarAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
		
		val paymentAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.payment_adj, defaultAdjustment)
		
		val maturityAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
	
		val calendar:Calendar = db.calendar
		
		val fixingInArrears:Boolean = db.inarrears.isDefined && db.inarrears == 0
		
		val couponNotice:Int = db.cpnnotice.getOrElse(5)
		
		val issuePrice:Option[Double] = db.issueprice
		
		val call:String = db.call
		
		val bondType:String = db.bondtype
		
		val initialFX:Double = db.initialfx
		
		val rule:DateGeneration.Rule = DateGeneration.Rule.Backward
		
		val firstDate:Option[qlDate] = None
		 
		val nextToLastdate:Option[qlDate] = None
		
		val issuer:String = db.issuerid
		
		val redemnotice = db.redemnotice.getOrElse(10)
		
		val schedule:Schedule = try {
			Schedule(issueDate, maturity, period, calendar, calendarAdjust, paymentAdjust, 
			    maturityAdjust, rule, fixingInArrears, couponNotice, daycount, firstDate, 
			    nextToLastdate, true, redemnotice)}
			  catch { case _ => null}
		      
		if (schedule == null) { println(db.id  + " : schedule cannot be initialized"); return None}

		val coupon:Payoffs = if (db.coupon == null || db.coupon.isEmpty) null
			else Payoffs(db.coupon, schedule.size - 1)
			
		val redemption:Payoff = Payoff(db.redemprice)
		
		if (coupon == null) {println(id + " : coupon not defined"); return None}
		if (coupon.size + 1 != schedule.size) {println(id + " : coupon (" + (coupon.size+1) + ") and schedule (" + schedule.size + ")  not compatible"); return None}
		
		val settings:Option[JsonNode] = db.settings.jsonNode
		
		Some(new Bond(db, id, issueDate, maturity, currency, nominal, denomination, period, daycount,	
			calendarAdjust,	paymentAdjust,	maturityAdjust, calendar, fixingInArrears, couponNotice,	
			issuePrice,	call, bondType,	initialFX,	issuer,	schedule, coupon, redemption, settings))
	  
	}
  
}
	
