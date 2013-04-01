package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Issuer(@Column("ID")					override var id: String,
              @Column("NAME")				var name: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_DISPLAY")	var name_jpn_display: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("ADDRESS")			var address: String,
              @Column("ADDRESS_LAT")		var address_lat: Option[Double],
              @Column("ADDRESS_LNG")		var address_lng: Option[Double],
              @Column("CountryID")			var countryid: String,
              @Column("TYPE")				var issuertype: String,
              @Column("RATING_MDY")			var rating_mdy: Option[Int],
              @Column("RATING_MDY_WATCH")	var rating_mdy_watch: String,
              @Column("RATING_SP")			var rating_SP: Option[Int],
              @Column("RATING_SP_WATCH")	var rating_SP_watch: String,
              @Column("RATING_FITCH")		var rating_FITCH: Option[Int],
              @Column("RATING_FITCH_WATCH")	var rating_FITCH_watch: String,
              @Column("BBG_EQTY")			var bbg_eqty: String,
              @Column("BBG_DEBT")			var bbg_debt: String,
              @Column("EDINET")				var edinet: String,
              @Column("EDINET_NAME")		var edinet_name: String,
              @Column("EDINET_ADDRESS")		var edinet_address: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("RISKTAGS") 			var risktags: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends StringEntity {
  
  def this() = this(
		id = null,
		name = null,
		name_jpn = null,
		name_jpn_display = null,
		name_jpn_short = null,
		address = null,
		address_lat = Some(-999.0),
		address_lng = Some(-999.0),
		countryid = null,
		issuertype = null,
		rating_mdy = Some(-999),
		rating_mdy_watch = null,
		rating_SP = Some(-999),
		rating_SP_watch = null,
		rating_FITCH = Some(-999),
		rating_FITCH_watch = null,
		bbg_eqty = null,
		bbg_debt = null,
		edinet = null,
		edinet_name = null,
		edinet_address = null,
		description_jpn = null,
		description_eng = null,
		risktags = "",
		created = None,
		lastmodified  = None)

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, name, name_jpn, address, issuertype, created.toString)
}
