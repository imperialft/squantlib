package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class FXRate(@Column("ID")					var id:Int,
              @Column("PARAMDATE")			var paramdate:Date,
              @Column("PARAMSET")			var paramset:String,
              @Column("CurrencyID")			var currencyid:String,
              @Column("FXRATE_JPY")			var fxjpy:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[Int] {
  
  def this() = this(
      id = -99999, 
      paramdate = new Date, 
      paramset = null,
      currencyid = null, 
      fxjpy = -99999.0, 
      lastmodified = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, paramdate, paramset, currencyid, fxjpy)
}
