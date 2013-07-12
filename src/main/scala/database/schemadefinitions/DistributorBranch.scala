package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

case class DistributorBranch(
			  @Column("ID")					override var id: Int,
              @Column("NAME")				var name: String,
              @Column("URL")				var url: String,
              @Column("ADDRESS")			var address: String,
              @Column("TEL")				var tel: String,
              @Column("HOURS")				var hours: String,
              @Column("DistributorID")		var distributorid: String,
              @Column("ADDRESS_LAT")		var addresslat: Option[Double],
              @Column("ADDRESS_LNG")		var addresslng: Option[Double]
              ) extends IntEntity {
  
  def this() = this(
	id = -999999,
	name = null,
	url = null,
	address = null,
	tel = null,
	hours = null,
	distributorid = null,
	addresslat = Some(-9999.9),
	addresslng = Some(-9999.9))

  override def toString():String = format("%-5s %-15s %-10s %-30s %-15s %-15s", id, name, url, address, tel, hours)
}
