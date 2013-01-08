package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.time.{Date => qlDate}
import squantlib.setting.initializer.Daycounters


class Coupon(@Column("ID")				var id:String,
			@Column("BondID")			var bondid:String,
			@Column("CurrencyID")		var currency:String,
			@Column("Rate")				var rate:String,
			@Column("EventDate")		var eventdate:Date,
			@Column("StartDate")		var startdate:Date,
			@Column("EndDate")			var enddate:Date,
			@Column("PaymentDate")		var paymentdate:Date,
			@Column("FixedRate")		var fixedrate:Option[BigDecimal],
			@Column("FixedAmount")		var fixedamount:Option[BigDecimal],
			@Column("SpotRate")			var spotrate:Option[BigDecimal],
			@Column("SpotAmount")		var spotamount:Option[BigDecimal],
			@Column("JsonFormat")		var jsonformat:String,
			@Column("Comment")			var comment:String,
			@Column("Daycount")			var daycount:String,
			@Column("PaymentType")		var paymenttype:String,
			@Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[String] {
  
  
  def this() = this(
      id = null,
      bondid = null,
      currency = null,
      rate = null,
      eventdate = null,
      startdate = null,
      enddate = null,
      paymentdate = null,
      fixedrate = Some(-999.0),
      fixedamount = Some(-999.0),
      spotrate = Some(-999.0),
      spotamount = Some(-999.0),
      jsonformat = null,
      comment = null,
      daycount = null,
      paymenttype = null,
      lastmodified = None)
      
  
  def isActive(valuedate:Date):Boolean = 
    (!valuedate.before(startdate) && enddate.after(valuedate) && paymenttype.toUpperCase != "REDEMPTION")
  
  def accruedCoupon(valuedate:Date):Option[Double] = {
    if (!isActive(valuedate)) None
    else fixedrate match {
      case None => None
      case Some(r) => {
        val dcf = Daycounters.getOrElse(daycount, new Actual365Fixed)
        Some(r.toDouble * dcf.yearFraction(new qlDate(startdate), new qlDate(valuedate)))
      }
    }
  }
      
  override def toString():String = format("%-15s %-5s %-15s %tF %tF %-10s %-10s", bondid, currency, rate, eventdate, paymentdate, fixedamount.getOrElse(""), comment)
      
}
