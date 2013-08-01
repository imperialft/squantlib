package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.time.{Date => qlDate}
import squantlib.util.initializer.Daycounters


class Callability(@Column("ID")				override var id:String,
			@Column("BondID")			var bondid:String,
			@Column("EventDate")		var eventdate:Date,
			@Column("ValueDate")		var valuedate:Date,
			@Column("Calltype")			var calltype:String,
			@Column("Trigger")			var trigger:String,
			@Column("Fixings")			var fixings:String,
			@Column("IsTriggered")		var istriggered:Int,
			@Column("RedemptionAmount")	var redemptionamount:BigDecimal,
			@Column("LastModified")		var lastmodified:Date
) extends StringEntity {
  
  def this() = this(
      id = null,
      bondid = null,
      eventdate = null,
      valuedate = null,
      calltype = null,
      trigger = null,
      fixings = null,
      istriggered = 0,
      redemptionamount = -9999,
      lastmodified = null)
      
  override def toString():String = format("%-10s %-10s %-10s %-10s %-10s %-10s %-15s %-10s", 
      bondid, 
      eventdate, 
      valuedate, 
      calltype, 
      trigger, 
      fixings, 
      istriggered, 
      redemptionamount)
      
}
