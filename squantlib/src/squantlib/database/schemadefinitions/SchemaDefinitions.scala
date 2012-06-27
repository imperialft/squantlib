package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column

class Country(var id:         String,
              @Column("currency_id")
              var currencyId: String,
              var name:       String,
              @Column("short_name")
              var shortName:  Option[String],
              var region:     String) {
  override def toString():String = "Country<ID=" + id + ", Name=" + name + ">"
}
