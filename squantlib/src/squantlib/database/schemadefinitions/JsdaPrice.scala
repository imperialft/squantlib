package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class JsdaPrice(@Column("ID")				var id: String,
			  @Column("BONDID")				var bondid:String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("JADAID")				var jsdaid: String,
              @Column("BONDNAME")			var bondname: String,
              @Column("PRICE")				var price: Double,
              @Column("COUPON")				var coupon: Double,
              @Column("COMPOUNDYIELD")		var compoundyield: Double,
              @Column("SIMPLEYIELD")		var simpleyield: Double,
              @Column("MATURITY")			var maturity: Date,
              @Column("reports_count")		var reportscount: Int,
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[String] {

  def this() = this(
      id = null, 
      bondid = null,
      paramdate = new Date, 
      jsdaid = null,
      bondname = null,
      price = -99999,
      coupon = -99999,
      compoundyield = -99999,
      simpleyield = -99999,
      maturity = new Date,
      reportscount = -99999,
      created = None)

  override def toString():String = format("%-11s %-15s %-10s %-10s %-15s %-15s %-15s", id, paramdate, bondid, price, maturity, simpleyield, compoundyield)
  
}
