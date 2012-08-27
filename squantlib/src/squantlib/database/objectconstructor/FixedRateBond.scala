package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.Bond
import squantlib.initializer.{Currencies, Daycounter}
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond }
import org.jquantlib.time.{Date => JDate, Period => JPeriod, TimeUnit, Schedule, DateGeneration}

object FixedRateBondConstructor {
  
	val productlist = List("SB", "STEPUP", "DISC")
	
	def ratetoarray(formula:String, size:Int):Array[Double] = {
		val numarray = formula.split(";").map(x => (try{x.replace("%", "").trim.toDouble / 100.0} catch { case _ => Double.NaN}))
		(0 to (size-1)).map(i => { val m = size - numarray.size; if(i < m) numarray(0) else numarray(i - m)}).toArray
	}
	
	def getbonds(bonds:Set[Bond]):Map[String, qlFixedRateBond] = {
	  bonds.map(b => (b.id, getbond(b))).filter(b => b._2 != null).toMap
	}
	
	def getbond(bond:Bond):qlFixedRateBond = {
	  val isvalid = productlist.contains(bond.productid) && !bond.coupon.isEmpty && !bond.coupon_freq.isEmpty && !bond.redemprice.isEmpty && !bond.daycount_adj.isEmpty
	  if (!isvalid) null
	  else {
		  		val bondid = bond.id
		  		val issuerid = bond.issuerid
		  		val issuedate = new JDate(bond.issuedate)
				val maturity = new JDate(bond.maturity)
				val schedule = {
				  val tenor = new JPeriod(bond.coupon_freq.get, TimeUnit.Months)
				  val calendar = bond.calendar
				  val convention = Daycounter.getdaycount_adj(bond.daycount_adj)
				  val maturityconvention = Daycounter.getdaycount_adj(bond.daycount_adj)
				  val rule = DateGeneration.Rule.Backward
				  val endofmonth = false
				  new Schedule(issuedate, maturity, tenor, calendar, convention, maturityconvention, rule, endofmonth)
				}
				
				val currency = Currencies.getcurrency(bond.currencyid)
				val settlementdays = 0
				val faceamount = 100.0
				val coupons:Array[Double] = ratetoarray(bond.coupon, schedule.size - 1)
				val accrualdaycounter = Daycounter.getdaycount(bond.daycount)
				val paymentconvention = Daycounter.getdaycount_adj(bond.payment_adj)
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