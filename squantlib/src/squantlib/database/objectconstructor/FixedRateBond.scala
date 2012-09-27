package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.initializer.{Currencies, DayAdjustments, Daycounters}
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond }
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration, BusinessDayConvention}
import org.jquantlib.daycounters.Actual365Fixed

object FixedRateBondConstructor {
  
	val productlist = List("SB", "STEPUP", "DISC")
	val defaultAdjustment = BusinessDayConvention.ModifiedFollowing
	val defaultDayCounter = new Actual365Fixed
	
	def ratetoarray(formula:String, size:Int):Array[Double] = {
		val numarray = formula.split(";").map(x => (
		    try{x.replace("%", "").trim.toDouble / 100.0} 
		    catch { case _ => Double.NaN }))
		    
		(0 to (size-1)).map(i => {
		  val m = size - numarray.size
		  if(i < m) numarray(0) else numarray(i - m)
		  }).toArray
	}
	
	def getbonds(bonds:Set[dbBond]):Map[String, qlFixedRateBond] = {
	  bonds.map(b => (b.id, getbond(b))).filter(b => b._2 != null).toMap
	}
	
	def getbond(bond:dbBond):qlFixedRateBond = {
	  val isvalidbond = productlist.contains(bond.productid) && 
			  		!bond.coupon.isEmpty && 
			  		!bond.coupon_freq.isEmpty && 
			  		!bond.redemprice.isEmpty && 
			  		!bond.daycount_adj.isEmpty
	  
	  if (!isvalidbond) null
	  else {
	  		val bondid = bond.id
	  		val issuerid = bond.issuerid
	  		val issuedate = new qlDate(bond.issuedate)
			val maturity = new qlDate(bond.maturity)
			val schedule = {
			  val tenor = new qlPeriod(bond.coupon_freq.get, TimeUnit.Months)
			  val calendar = bond.calendar
			  val convention = DayAdjustments.getOrElse(bond.daycount_adj, defaultAdjustment)
			  val maturityconvention = DayAdjustments.getOrElse(bond.daycount_adj, defaultAdjustment)
			  val rule = DateGeneration.Rule.Backward
			  val endofmonth = false
			  new Schedule(issuedate, maturity, tenor, calendar, convention, maturityconvention, rule, endofmonth)
			}
			
			val currency = Currencies.getOrElse(bond.currencyid, null)
			val settlementdays = 0
			val faceamount = 100.0
			val coupons:Array[Double] = ratetoarray(bond.coupon, schedule.size - 1)
			val accrualdaycounter = Daycounters.getOrElse(bond.daycount, defaultDayCounter)
			val paymentconvention = DayAdjustments.getOrElse(bond.payment_adj, defaultAdjustment)
			val redemption = try{bond.redemprice.trim.toDouble} catch { case _ => Double.NaN}
			val initialfx = bond.initialfx
			
			new qlFixedRateBond(settlementdays, 
			    faceamount, 
			    schedule, 
			    coupons, 
			    accrualdaycounter, 
			    paymentconvention, 
			    redemption, 
			    issuedate, 
			    bondid, 
			    currency, 
			    issuerid, 
			    initialfx)
	  	}
	}
	
}