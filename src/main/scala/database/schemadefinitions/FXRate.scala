package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class FXRate(@Column("ID")					override var id:Int,
              @Column("PARAMDATE")			var paramdate:Date,
              @Column("PARAMSET")			var paramset:String,
              @Column("CurrencyID")			var currencyid:String,
              @Column("FXRATE_JPY")			var fxjpy:Double,
              @Column("LastModified")		var lastmodified:Option[Date],
              @Column("COMMENT")			var comment:String
              ) extends IntEntity {
  
  def this() = this(
      id = -99999, 
      paramdate = new Date, 
      paramset = null,
      currencyid = null, 
      fxjpy = -99999.0, 
      lastmodified = None,
      comment = null)

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, paramset, currencyid, fxjpy, comment)
}
