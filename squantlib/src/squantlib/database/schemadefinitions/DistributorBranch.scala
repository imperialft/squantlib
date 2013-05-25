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
              @Column("DistributorID")		var distributorid: String
              ) extends IntEntity {
  
  def this() = this(
	id = -999999,
	name = null,
	url = null,
	address = null,
	tel = null,
	hours = null,
	distributorid = null)

  override def toString():String = format("%-5s %-15s %-10s %-30s %-15s %-15s", id, name, url, address, tel, hours)
}
