package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.Bond
import squantlib.database.schemadefinitions.DbKeywords
import squantlib.model.currencies.CurrencyConversion
import org.jquantlib.instruments.bonds.FixedRateBond
import org.jquantlib.time.{Date => JDate, Period => JPeriod, TimeUnit, Schedule, DateGeneration}

object FixedRateBondConstructor {
  
	val productid = "SB"
	
	def ratetoarray(formula:String, size:Int) = {
		val numarray = formula.split(";").map(x => (try{x.trim.toDouble / 100.0} catch { case _ => Double.NaN}))
		(0 to (size-1)).map(i => { val m = size - numarray.size; if(i < m) numarray(0) else numarray(i - m)}).toArray
	}
	
	def getbonds(bonds:Set[Bond]):Map[String, FixedRateBond] = {
	  val validbonds = bonds.filter(b => (b.productid == productid && !b.coupon.isEmpty && !b.coupon_freq.isEmpty && !b.redemprice.isEmpty))
	  
	  validbonds.map(b => { 
		  	val issuedate = new JDate(b.issuedate)
			val maturity = new JDate(b.maturity)
			val schedule = {
			  val tenor = new JPeriod(b.coupon_freq.get, TimeUnit.Months)
			  val calendar = CurrencyConversion.getcalendar(b.currencyid)
			  val convention = DbKeywords.daycount_adj(b.daycount_adj)
			  val maturityconvention = DbKeywords.daycount_adj(b.daycount_adj)
			  val rule = DateGeneration.Rule.Backward
			  val endofmonth = false
			  new Schedule(issuedate, maturity, tenor, calendar, convention, maturityconvention, rule, endofmonth)
			}
			
			val currency = CurrencyConversion.getcurrency(b.currencyid)
			val settlementdays = 0
			val faceamount = 100.0
			val coupons:Array[Double] = ratetoarray(b.coupon, schedule.size)
			val accrualdaycounter = DbKeywords.daycount(b.daycount)
			val paymentconvention = DbKeywords.daycount_adj(b.payment_adj)
			val redemption = try{b.redemprice.trim.toDouble} catch { case _ => Double.NaN}
			
			(b.id, new FixedRateBond(settlementdays, faceamount, schedule, coupons, accrualdaycounter, paymentconvention, redemption, issuedate))
	  	}).toMap
	}
	
}