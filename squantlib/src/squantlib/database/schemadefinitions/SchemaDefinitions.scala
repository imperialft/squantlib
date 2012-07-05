package squantlib.database.schemadefinitions

import java.util.Date
import java.util.Formatter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column
import org.squeryl.dsl.{ManyToOne, ManyToMany, OneToMany}
import org.squeryl.KeyedEntity
import squantlib.database.DB
import squantlib.database.objectconstructor._

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
              ) extends KeyedEntity[String] {
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
              ) extends KeyedEntity[String] {
  def this() = this(null, null, null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, name_eng, name_jpn, created.toString)
}

class Distributor(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
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
              ) extends KeyedEntity[String] {
  def this() = this(null, null, null, null, null, null, null, null, Some(0.0), Some(0.0), null, null, null,  null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-10s %-30s %-15s %-15s", id, name_jpn, post, address, tel, created.toString)
}


class Issuer(@Column("ID")					var id: String,
              @Column("NAME")				var name: String,
              @Column("NAME_JPN")			var name_jpn: String,
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
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  def this() = this(null, null, null, null, null, Some(0.0), Some(0.0), null, null, Some(0), null, Some(0), null, Some(0), null, null, null, null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, name, name_jpn, address, issuertype, created.toString)
}


class Product(@Column("ID")					var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("TYPE")				var producttype: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  def this() = this(null, null, null, null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, name_eng, name_jpn, producttype, producttype, created.toString)
}


class Bond(@Column("ID")					var id: String,
              @Column("REF_NUMBER")			var ref_number: Int,
              @Column("FILING")				var filing: Date,
              @Column("ISSUEDATE")			var issuedate: Date,
              @Column("MATURITY")			var maturity: Date,
              @Column("NOMINAL")			var nominal: Double,
              @Column("DENOMINATION")		var denomination: Option[Double],
              @Column("COUPON")				var coupon: String,
              @Column("COUPON_FREQ")		var coupon_freq: Option[Int],
              @Column("DAYCOUNT")			var daycount: String,
              @Column("DAYCOUNT_ADJ")		var daycount_adj: String,
              @Column("PAYMENT_ADJ")		var payment_adj: String,
              @Column("INARREARS")			var inarrears: Option[Int],
              @Column("CPNNOTICE")			var cpnnotice: Option[Int],
              @Column("ISSUEPRICE")			var issueprice: Option[Double],
              @Column("REDEMPRICE")			var redemprice: String,
              @Column("CALL")				var call: String,
              @Column("TYPE")				var bondtype: String,
              @Column("INITIALFX")			var initialfx: Double,
              @Column("ISIN")				var isin: String,
              @Column("TICKER")				var ticker: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("ProductID")			var productid: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {

  def this() = this(null, 0, new Date, new Date, new Date, 0.0, Some(0.0), null, Some(0), null, null, null, Some(0), Some(0), Some(0.0), null, null, null, 0.0, null, null, null, null, null, null, null, Some(new Date), Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)
  
  def toFixedRateBond = FixedRateBondConstructor.getbond(this)
}


class InputParameter(@Column("ID")			var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("ASSET")				var asset: String,
              @Column("MATURITY")			var maturity: String,
              @Column("VALUE")				var value: Double,
              @Column("OPTION")				var option: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[Int] {
  def this() = this(-99999, null, new Date, null, null, null, -99999, null, null, Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, asset, maturity, value)
}


class CDSParameter(@Column("ID")			var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("SPREAD")				var spread: Double,
              @Column("MATURITY")			var maturity: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[Int] {
  def this() = this(-99999, null, new Date, null, null, null, -99999.0, null, null, Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, issuerid, currencyid, spread)
  
  def toCDSCurve = CDSCurveConstructor.getcurve(this)
}


