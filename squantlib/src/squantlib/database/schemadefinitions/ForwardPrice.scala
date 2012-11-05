package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class ForwardPrice(@Column("ID")			var id: String,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("ValueDate")			var valuedate: Date,
              @Column("UnderlyingID")		var underlying: String,
              @Column("VALUE")				var value: Double,
              @Column("VALUEJPY")			var valuejpy: Option[Double],
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      paramset = null, 
      paramdate = null, 
      valuedate = null, 
      underlying = null, 
      value = -99999, 
      valuejpy = Some(-999.0), 
      created = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, paramset, valuedate, underlying, value)
}
