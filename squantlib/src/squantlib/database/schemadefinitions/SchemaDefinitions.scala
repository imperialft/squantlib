package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column

class Country(@Column("ID")				var id: String,
              @Column("CurrencyID")		var currencyid: String,
              @Column("NAME_JPN")		var name_jpn: String,
              @Column("NAME_ENG")		var name_eng: String,
              @Column("REGION")			var region: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("ADDRESS_LAT")	var address_lat: Option[Double],
              @Column("ADDRESS_LNG")	var address_lng: Option[Double],
              @Column("Created")	var created: Option[Date],
              @Column("LastModified")	var lastmodified : Option[Date]
              ) {
  override def toString():String = "Country<ID=" + id + ", Name=" + name_eng + " " + name_jpn+ ">"
  
  def this() = this(null, null, null, null, null, null, null, Some(0.0), Some(0.0), Some(new Date), Some(new Date))
}
