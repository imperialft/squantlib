package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

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
  
  def this() = this(
	id = null,
	name_jpn = null,
	name_jpn_short = null,
	name_eng = null,
	name_eng_short = null,
	address = null,
	post = null,
	tel = null,
	address_lat = Some(-999.0),
	address_lng = Some(-999.0),
	fsa_region = null,
	fsa_id = null,
	fsa_name = null,
	fsa_post = null,
	fsa_address = null,
	fsa_tel = null,
	description_jpn = null,
	description_eng = null,
	created = None,
	lastmodified  = None)

  override def toString():String = format("%-5s %-15s %-10s %-30s %-15s %-15s", id, name_jpn, post, address, tel, created.toString)
}
