package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Volatility(@Column("ID")				var id:String,
              @Column("UnderlyingID")		var underlying:String,
              @Column("ValueDate")			var valuedate:Date,
              @Column("Periodicity")		var periodicity:Int,
              @Column("NbDays")				var nbdays:Int,
              @Column("Value")				var value:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      underlying = null,
      valuedate = null,
      periodicity = -999999,
      nbdays = -99999,
      value = -99999999,
      lastmodified = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, underlying, valuedate, periodicity, value)
}
