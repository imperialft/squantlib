package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Equity(@Column("ID")					override var id:String,
              @Column("CurrencyID")			var currencyid:String,
              @Column("DIV_BASEDATE")		var basedivdate:Date,
              @Column("DIV_FREQ")			var divfreq:Int
              ) extends StringEntity {
  
  def this() = this(
      id = null, 
      currencyid = null, 
      basedivdate = null,
      divfreq = -99999)

  override def toString():String = format("%-5s %-5s %-15s %-15s", id, currencyid, basedivdate, divfreq)
}
