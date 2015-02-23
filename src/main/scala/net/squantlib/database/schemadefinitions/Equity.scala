package net.squantlib.database.schemadefinitions

import java.sql.Timestamp
import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Equity(@Column("ID")					override var id:String,
              @Column("CurrencyID")			var currencyid:String,
              @Column("DIV_BASEDATE")		var basedivdate:Date,
              @Column("DIV_FREQ")			var divfreq:Int,
              @Column("VOLATILITY")			var volatility:Double,
              @Column("PARAMDATE")			var paramdate:Date,
              @Column("Created")			override var created: Timestamp,
              @Column("LastModified")		override var lastmodified : Timestamp
              ) extends StringEntity {
  
  def this() = this(
      id = null, 
      currencyid = null, 
      basedivdate = null,
      divfreq = -99999,
      volatility = -999999,
      paramdate = null,
      created = null,
      lastmodified = null
      )

  override def toString():String = "%-5s %-5s %-15s %-15s".format(id, currencyid, basedivdate, divfreq)
}
