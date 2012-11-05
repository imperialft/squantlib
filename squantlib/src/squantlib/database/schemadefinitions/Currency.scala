package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

class Currency(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("RISKTAGS") 			var risktags: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      name_jpn = null, 
      name_jpn_short = null, 
      name_eng = null, 
      name_eng_short = null, 
      description_jpn = null, 
      description_eng = null,
      risktags = "",
      created = None, 
      lastmodified = None)

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, name_eng, name_jpn, created.toString)
}
