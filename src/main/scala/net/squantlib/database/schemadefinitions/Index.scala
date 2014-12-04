package net.squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Index(@Column("ID")					override var id:String,
              @Column("CurrencyID")			var currencyid:String,
              @Column("VOLATILITY")			var volatility:Double,
              @Column("PARAMDATE")			var paramdate:Date,
              @Column("created_at")			var created:Option[Date],
              @Column("updated_at")			var lastmodified:Option[Date]
              ) extends StringEntity {
  
  def this() = this(
      id = null, 
      currencyid = null, 
      volatility = -999999,
      paramdate = null,
      created = None,
      lastmodified = None)

  override def toString():String = "%-5s %-5s %-15s %-15s".format(id, currencyid, paramdate, created)
}
