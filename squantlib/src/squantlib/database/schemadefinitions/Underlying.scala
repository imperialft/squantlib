package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class Underlying(@Column("ID")				var id:String,
              @Column("NAME")				var name:String,
              @Column("NAME_JPN")			var namejpn:String,
              @Column("MULTIPLE")			var multiple:Int,
              @Column("FORMAT")				var stringformat:String,
              @Column("UNIT")				var unit:String
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      name = null, 
      namejpn = null,
      multiple = -99999, 
      stringformat = null, 
      unit = null)
      
  def display(v:Double):String = stringformat.format(v * multiple.toDouble) + unit

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, name, namejpn, display(1.00))
}

