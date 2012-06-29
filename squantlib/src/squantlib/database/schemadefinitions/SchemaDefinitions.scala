package squantlib.database.schemadefinitions

import java.util.Date
import java.util.Formatter
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
  def this() = this(null, null, null, null, null, null, null, Some(0.0), Some(0.0), Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, name_eng, name_jpn, region, created.toString)
}

class Currency(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) {
  def this() = this(null, null, null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, name_eng, name_jpn, created.toString)
}

class Distributor(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("URL")				var url: String,
              @Column("ADDRESS")			var address: String,
              @Column("POST")				var post: String,
              @Column("TEL")				var tel: String,
              @Column("ADDRESS_LAT")		var address_lat: Option[Double],
              @Column("ADDRESS_LNG")		var address_lng: Option[Double],
              @Column("FSA_REGION")			var fsa_region: String,
              @Column("FSA_ID")				var fsa_id: String,
              @Column("FSA_NAME")			var fsa_name: String,
              @Column("FSA_POST")			var fsa_post: String,
              @Column("FSA_ADDRESS")		var fsa_address: String,
              @Column("FSA_TEL")			var fsa_tel: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) {
  def this() = this(null, null, null, null, null, null, null, null, null, Some(0.0), Some(0.0), null, null, null,  null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-30s %-15s %-15s", id, name_jpn, url, post, address, tel, created.toString)
}












