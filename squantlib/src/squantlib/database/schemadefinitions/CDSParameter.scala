package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity
import squantlib.model.rates.CDSCurve

class CDSParameter(@Column("ID")			override var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("SPREAD")				var spread: Double,
              @Column("MATURITY")			var maturity: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends IntEntity {
  
  
  def this() = this(
      id = -99999, 
      paramset = null, 
      paramdate = new Date, 
      instrument = null, 
      issuerid = null, 
      currencyid = null, 
      spread = -99999.0, 
      maturity = null, 
      comment = null, 
      created = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, issuerid, currencyid, spread)
  
}
