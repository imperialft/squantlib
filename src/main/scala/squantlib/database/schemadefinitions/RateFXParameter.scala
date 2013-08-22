package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.jquantlib.time.{Date => qlDate}

class RateFXParameter(@Column("ID")			override var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("ASSET")				var asset: String,
              @Column("MATURITY")			var maturity: String,
              @Column("VALUE")				var value: Double,
              @Column("OPTION")				var option: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends IntEntity {
  
  def this() = this(
      id = 0, 
      paramset = null, 
      paramdate = new Date, 
      instrument = null, 
      asset = null, 
      maturity = null, 
      value = -99999, 
      option = null, 
      comment = null, 
      created = None)

  override def toString():String = format("%-15s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, asset, maturity, value)
  
  def toMaturityValuePair = Map(maturity -> value)
}


object RateFXParameter {
 
  def apply(id: Int,
            paramset: String,
            paramdate: qlDate,
            instrument: String,
            asset: String,
            maturity: String,
            value: Double,
            option: String,
            comment: String,
            created: Option[qlDate]) 
  	= new RateFXParameter(
  	    id, 
  	    paramset, 
  	    paramdate.longDate, 
  	    instrument, 
  	    asset, 
  	    maturity, 
  	    value, 
  	    option, 
  	    comment, 
  	    created.collect{case d => d.longDate})
  
}