package squantlib.database.objectconstructor

import squantlib.database.fixings.Fixings
import squantlib.montecarlo.payoff.GeneralPayoff
import squantlib.setting.initializer.{Daycounters, DayAdjustments}
import squantlib.database.schemadefinitions.{Bond => dbBond, Coupon => dbCoupon}
import squantlib.montecarlo.payoff.FixedPayoff
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration}
import org.jquantlib.time.BusinessDayConvention.{ModifiedFollowing, Unadjusted}
import org.jquantlib.daycounters.Thirty360
import java.util.{Date => JavaDate}

object Coupon {
  
	private implicit def dateconversion_javaql(jdate:JavaDate):qlDate = new qlDate(jdate)
	private implicit def dateconversion_qlJava(jdate:qlDate):JavaDate = jdate.longDate
	
	def apply(bond:dbBond):List[dbCoupon] = build(bond)
	def currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	def ratetoarray(formula:String, size:Int):Array[String] = {
		val arrayedschedule = formula.split(";")
		(0 to (size-1)).map(i => { 
		  val m = size - arrayedschedule.size; 
		  if(i < m) arrayedschedule(0) else arrayedschedule(i - m)}).toArray
	}
	
	def parseFixCoupon(formula:String):Option[Double] = 
	  try Some(formula.replace("%", "").trim.toDouble / 100.0) catch {case _ => None}
	
	def getbonds(bonds:Set[dbBond]):Map[String, List[dbCoupon]] = 
	  bonds.map(b => (b.id, build(b))).toMap
	
	   
	def update(cpn:dbCoupon):dbCoupon = {
	  val fixcpn = parseFixCoupon(cpn.rate)
	  
	  val (fixedrate, comment) = 
	    if (fixcpn.isDefined) (fixcpn, null)
		else {
			  val interpreter = new GeneralPayoff(cpn.rate)
			  val varfixings = interpreter.variables.map(v => (v -> Fixings(v, cpn.eventdate))).toMap
			  
			  if (!varfixings.forall(_._2.isDefined)) (None, null)
			  else {
			    val fixedrate = interpreter.price(varfixings.mapValues(_.get._2))
			    val cmt:String = (if (cpn.comment == null) "" else cpn.comment) + 
			    		varfixings.map{
			    			case (k, Some((v1, v2))) => k + "=" + "%.4f".format(v2)
			    			case (k, None) => ""
			    			}.mkString(", ")
			    println(cpn.bondid + " " + cpn.eventdate + " " + cpn.rate + " fixed:" + fixedrate.collect{case v => "%.4f".format(v)}.orNull + " " + cmt)
				(fixedrate, cmt)
			  }
		  }
	  
	  if (fixedrate.isEmpty) return cpn
	  
	  val daycount = Daycounters.getOrElse(cpn.daycount, new Thirty360)
	  val fixedamount = Some(fixedrate.get * daycount.yearFraction(cpn.startdate, cpn.enddate))
	  
	  new dbCoupon(
		      id = cpn.id,
		      bondid = cpn.bondid,
		      currency = cpn.currency, 
		      rate = cpn.rate,
		      eventdate = cpn.eventdate,
		      startdate = cpn.startdate,
		      enddate = cpn.enddate,
		      paymentdate = cpn.paymentdate,
		      fixedrate = fixedrate,
		      fixedamount = fixedamount,
		      comment = comment,
		      daycount = cpn.daycount,
		      paymenttype = "COUPON",
		      lastmodified = Some(currenttime))
	}
	
	def build(bond:dbBond):List[dbCoupon] = {
	  if (!bond.coupon_freq.isDefined || 
	      bond.maturity == null || 
	      bond.coupon == null ||
	      bond.daycount == null ||
	      bond.daycount_adj == null) return List.empty
		
  		val bondid = bond.id
  		val issuerid = bond.issuerid
  		val issuedate:qlDate = bond.issuedate
		val maturity:qlDate = bond.maturity
  		
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
		val calcadjust = DayAdjustments.getOrElse(bond.daycount_adj, ModifiedFollowing)
		val payadjust = DayAdjustments.getOrElse(bond.payment_adj, ModifiedFollowing)
		val daycount = Daycounters.getOrElse(bond.daycount, new Thirty360)
		val calendar = bond.calendar
		
		val redemption = FixedPayoff(bond.redemprice).price.head
		
		var cpnlist = (0 to (baseschedule.size - 2)).map ( i => {
		  val cpnformula = ratearray(i)
		  val fixcpn = parseFixCoupon(cpnformula)
	      val startdate = calendar.adjust(baseschedule.date(i), calcadjust)
	      val enddate = calendar.adjust(baseschedule.date(i+1), calcadjust)
	      val paymentdate = calendar.adjust(baseschedule.date(i+1), payadjust)
	      
	      val (fixedrate, fixedamount) = fixcpn match {
		    case Some(r) => (Some(r), Some(r * daycount.yearFraction(startdate, enddate)))
		    case None => (None, None)
		  }
	      
	      val eventdate = if (fixcpn.isDefined) issuedate
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
		      eventdate = eventdate,
		      startdate = startdate,
		      enddate = enddate,
		      paymentdate = paymentdate,
		      fixedrate = fixedrate,
		      fixedamount = fixedamount,
		      comment = null,
		      daycount = bond.daycount,
		      paymenttype = "COUPON",
		      lastmodified = Some(currenttime))
			}
		).toList
		
		cpnlist ++= List(new dbCoupon(
	      id = bond.id + ":" + cpnlist.size + ":REDEMPTION",
	      bondid = bond.id,
	      currency = bond.currencyid,
	      rate = bond.redemprice,
	      eventdate = bond.calendar.advance(
	          bond.maturity,
	          - bond.cpnnotice.getOrElse(0),
	          org.jquantlib.time.TimeUnit.Days),
	      startdate = bond.issuedate,
	      enddate = bond.maturity,
	      paymentdate = bond.calendar.adjust(bond.maturity),
	      fixedrate = redemption,
	      fixedamount = redemption,
	      comment = null,
	      daycount = "ABSOLUTE",
	      paymenttype = "REDEMPTION",
	      lastmodified = Some(currenttime)
	      ))
		
	      cpnlist
	}
	
}