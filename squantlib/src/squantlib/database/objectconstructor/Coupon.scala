package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.{Bond => dbBond, Coupon => dbCoupon}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration, BusinessDayConvention}
import squantlib.initializer.Daycounter

object Coupon {
  
	def ratetoarray(formula:String, size:Int):Array[(String, Double)] = {
		val arrayedschedule = formula.split(";").map(x => (
		    try { (x, x.replace("%", "").trim.toDouble / 100.0) } 
		    catch { case _ => (x, Double.NaN) }))
		(0 to (size-1)).map(i => { val m = size - arrayedschedule.size; if(i < m) arrayedschedule(0) else arrayedschedule(i - m)}).toArray
	}
	
	def getbonds(bonds:Set[dbBond]):Map[String, List[dbCoupon]] = 
	  bonds.map(b => (b.id, build(b))).toMap
	
	def build(bond:dbBond):List[dbCoupon] = {
	  if (!bond.coupon_freq.isDefined || 
	      bond.maturity == null || 
	      bond.coupon == null ||
	      bond.daycount == null ||
	      bond.daycount_adj == null) return null
		
  		val bondid = bond.id
  		val issuerid = bond.issuerid
  		val issuedate = new qlDate(bond.issuedate)
		val maturity = new qlDate(bond.maturity)
  		
	  	val baseschedule = new Schedule(
		    issuedate, 
		    maturity,
		    new qlPeriod(bond.coupon_freq.get, TimeUnit.Months),
		    bond.calendar,
		    BusinessDayConvention.Unadjusted,
		    BusinessDayConvention.Unadjusted,
		    DateGeneration.Rule.Backward,
		    false
		    )
	  
		
		val accrualdaycounter = Daycounter.getdaycount(bond.daycount)
		
		val redemption = try{bond.redemprice.trim.toDouble} catch { case _ => Double.NaN}
		val ratearray = ratetoarray(bond.coupon, baseschedule.size - 1)
		val calcadjust = Daycounter.getdaycount_adj(bond.daycount_adj)
		val payadjust = Daycounter.getdaycount_adj(bond.payment_adj)
		val daycount = Daycounter.getdaycount(bond.daycount)
		val calendar = bond.calendar
		
		var cpnlist = (0 to (baseschedule.size - 2)).map { i => {
	      val startdate = calendar.adjust(baseschedule.date(i), calcadjust)
	      val enddate = calendar.adjust(baseschedule.date(i+1), calcadjust)
	      val paymentdate = calendar.adjust(baseschedule.date(i+1), payadjust)
		  new dbCoupon(
		      id = bond.id + ":" + i + ":COUPON",
		      bondid = bond.id,
		      currency = bond.currencyid, 
		      rate = ratearray(i)._1,
		      eventdate = bond.calendar.advance(
		          if (bond.inarrears.getOrElse(1) == 1) startdate else enddate,
		          - bond.cpnnotice.getOrElse(0),
		          org.jquantlib.time.TimeUnit.Days).longDate,
		      startdate = startdate.longDate,
		      enddate = enddate.longDate,
		      paymentdate = paymentdate.longDate,
		      fixedamount = if (ratearray(i)._2.isNaN) None
		      			else Some(ratearray(i)._2 * daycount.yearFraction(startdate, enddate)),
		      comment = null,
		      daycount = bond.daycount,
		      paymenttype = "COUPON",
		      lastmodified = Some(java.util.Calendar.getInstance.getTime)
		      )
			}
		}.toList
		
		cpnlist ++= List(new dbCoupon(
	      id = bond.id + ":" + cpnlist.size + ":REDEMPTION",
	      bondid = bond.id,
	      currency = bond.currencyid,
	      rate = bond.redemprice,
	      eventdate = bond.calendar.advance(
	          new qlDate(bond.maturity),
	          - bond.cpnnotice.getOrElse(0),
	          org.jquantlib.time.TimeUnit.Days).longDate,
	      startdate = bond.issuedate,
	      enddate = bond.maturity,
	      paymentdate = bond.calendar.adjust(new qlDate(bond.maturity)).longDate,
	      fixedamount = try { Some(bond.redemprice.replace("%", "").trim.toDouble / 100.0)} catch { case _ => None },
	      comment = null,
	      daycount = "ABSOLUTE",
	      paymenttype = "REDEMPTION",
	      lastmodified = Some(java.util.Calendar.getInstance.getTime)
	      ))
		
	      cpnlist
	}
	
}