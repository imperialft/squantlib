package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

class Correlation(@Column("ID")				override var id:String,
              @Column("UnderlyingID")		var underlying1:String,
              @Column("Underlying1Asset")	var underlying1asset:String,
              @Column("Underlying1Type")	var underlying1type:String,
              @Column("Underlying1Name")	var underlying1name:String,
              @Column("Underlying2ID")		var underlying2:String,
              @Column("Underlying2Asset")	var underlying2asset:String,
              @Column("Underlying2Type")	var underlying2type:String,
              @Column("Underlying2Name")	var underlying2name:String,
              @Column("ValueDate")			var valuedate:Date,
              @Column("Periodicity")		var periodicity:Int,
              @Column("NbDays")				var nbdays:Int,
              @Column("Value")				var value:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends StringEntity {
  
  def this() = this(
      id = null, 
      underlying1 = null,
      underlying1asset = null,
      underlying1type = null,
      underlying1name = null,
      underlying2 = null,
      underlying2asset = null,
      underlying2type = null,
      underlying2name = null,
      valuedate = null,
      periodicity = -999999,
      nbdays = -99999,
      value = -99999999,
      lastmodified = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, underlying1, underlying2, value) + "%tY/%<tm/%<td".format(valuedate)
}
