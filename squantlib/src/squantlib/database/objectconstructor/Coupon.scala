package squantlib.database.objectconstructor

import squantlib.initializer.{Daycounters, DayAdjustments}
import squantlib.database.schemadefinitions.{Bond => dbBond, Coupon => dbCoupon}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration}
import org.jquantlib.time.BusinessDayConvention.{ModifiedFollowing, Unadjusted}
import org.jquantlib.daycounters.Thirty360

object Coupon {
  
	def apply(bond:dbBond):List[dbCoupon] = build(bond)
	
	def ratetoarray(formula:String, size:Int):Array[(String, Option[Double], Boolean)] = {
		val arrayedschedule = formula.split(";").map(x => {
		  val fixvalue:Option[Double] = try Some(x.replace("%", "").trim.toDouble / 100.0)
				  catch {case _ => None}
		  val fixingvalue:Option[Double] = if (fixvalue.isDefined) fixvalue else None
		  (x, fixingvalue, fixvalue.isDefined)})
		    
		(0 to (size-1)).map(i => { 
		  val m = size - arrayedschedule.size; 
		  if(i < m) arrayedschedule(0) else arrayedschedule(i - m)}).toArray
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
		    Unadjusted,
		    Unadjusted,
		    DateGeneration.Rule.Backward,
		    false
		    )
	  
		
		val accrualdaycounter = Daycounters(bond.daycount)
		
		val ratearray = ratetoarray(bond.coupon, baseschedule.size - 1)
		val calcadjust = DayAdjustments(bond.daycount_adj).getOrElse(ModifiedFollowing)
		val payadjust = DayAdjustments(bond.payment_adj).getOrElse(ModifiedFollowing)
		val daycount = Daycounters(bond.daycount).getOrElse(new Thirty360)
		val calendar = bond.calendar
		
		val redemption = try Some(bond.redemprice.replace("%", "").trim.toDouble / 100.0) 
						catch { case _ => None }
		
		
		var cpnlist = (0 to (baseschedule.size - 2)).map ( i => {
		  val (cpnformula, cpnrate, isfixcpn) = ratearray(i)
	      val startdate = calendar.adjust(baseschedule.date(i), calcadjust)
	      val enddate = calendar.adjust(baseschedule.date(i+1), calcadjust)
	      val paymentdate = calendar.adjust(baseschedule.date(i+1), payadjust)
	      
	      val (fixedrate, fixedamount) = cpnrate match {
		    case Some(r) => (Some(r), Some(r * daycount.yearFraction(startdate, enddate)))
		    case None => (None, None)
		  }
	      
	      val eventdate = if (isfixcpn) issuedate
		      else bond.inarrears match {
			        case Some(1) | None => bond.calendar.advance(enddate, -bond.cpnnotice.getOrElse(0), TimeUnit.Days)
			        case Some(0) => bond.calendar.advance(startdate, -bond.cpnnotice.getOrElse(0), TimeUnit.Days)
			        case _ => null
			      }
	      
		  new dbCoupon(
		      id = bond.id + ":" + i + ":COUPON",
		      bondid = bond.id,
		      currency = bond.currencyid, 
		      rate = cpnformula,
		      eventdate = eventdate.longDate,
		      startdate = startdate.longDate,
		      enddate = enddate.longDate,
		      paymentdate = paymentdate.longDate,
		      fixedrate = fixedrate,
		      fixedamount = fixedamount,
		      comment = null,
		      daycount = bond.daycount,
		      paymenttype = "COUPON",
		      lastmodified = Some(java.util.Calendar.getInstance.getTime)
		      )
			}
		).toList
		
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
	      fixedrate = redemption,
	      fixedamount = redemption,
	      comment = null,
	      daycount = "ABSOLUTE",
	      paymenttype = "REDEMPTION",
	      lastmodified = Some(java.util.Calendar.getInstance.getTime)
	      ))
		
	      cpnlist
	}
	
}