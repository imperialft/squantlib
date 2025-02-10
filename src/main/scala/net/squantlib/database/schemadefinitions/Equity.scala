package net.squantlib.database.schemadefinitions

import java.sql.Timestamp
import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Equity(@Column("id")					override var id:String,
              @Column("currency_id")			var currencyid:String,
              @Column("dividend_start_on")		var basedivdate:Date,
              @Column("dividend_freq")			var divfreq:Int,
              @Column("volatility")			var volatility:Double,
              @Column("value_on")			var paramdate:Date,
              @Column("settings")      var settings:String,
              @Column("created_at")			override var created: Timestamp,
              @Column("updated_at")		override var lastmodified : Timestamp
              ) extends StringEntity {
  
  def this() = this(
      id = null, 
      currencyid = null, 
      basedivdate = null,
      divfreq = -99999,
      volatility = -999999,
      paramdate = null,
      settings = null,
      created = null,
      lastmodified = null
      )

  override def toString():String = "%-5s %-5s %-15s %-15s".format(id, currencyid, basedivdate, divfreq)
}
