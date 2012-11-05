package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

class Country(@Column("ID")				var id: String,
              @Column("CurrencyID")		var currencyid: String,
              @Column("NAME_JPN")		var name_jpn: String,
              @Column("NAME_ENG")		var name_eng: String,
              @Column("REGION")			var region: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("ADDRESS_LAT")	var address_lat: Option[Double],
              @Column("ADDRESS_LNG")	var address_lng: Option[Double],
              @Column("RISKTAGS") 		var risktags: String,
              @Column("Created")		var created: Option[Date],
              @Column("LastModified")	var lastmodified: Option[Date]
              ) extends KeyedEntity[String] {

  def this() = this(
    id = null,
    currencyid = null,
    name_jpn = null,
    name_eng = null,
    region = null,
    description_jpn = null,
    description_eng = null,
    address_lat = Some(-999.0),
    address_lng = Some(-999.0),
    risktags = "",
    created = None,
    lastmodified = None
  )

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, name_eng, name_jpn, region, created.toString)
}