package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class HistoricalPrice(
			@Column("ID")				override var id:String, 
			@Column("BondID")			var bondid:String,
			@Column("PARAMDATE")		var paramdate:Date,
			@Column("CurrencyID")		var currencyid:String,
			@Column("FXJPY")			var fxjpy:Double,
			@Column("PRICEDIRTY")		var pricedirty:Double,
			@Column("PRICECLEAN")		var priceclean:Double,
			@Column("PRICEDIRTY_JPY")	var pricedirty_jpy:Double,
			@Column("PRICECLEAN_JPY")	var priceclean_jpy:Double,
			@Column("PRICETYPE")		var pricetype:String,
			@Column("Created")			var created:Date
			) extends StringEntity {
  
  def getFieldMap:Map[String, Any] = getObjectFieldMap(this)
  
  def isPriced = !(HistoricalPrice.noPriceKeys contains pricetype)
  
  def isMatured = HistoricalPrice.maturedKeys contains pricetype
  
  def isError = HistoricalPrice.errorKeys contains pricetype
  
  def isNotIssued = HistoricalPrice.notIssuedKeys contains pricetype
    
  def this() = this(
		id = null,
		bondid = null,
		currencyid = null,
		paramdate = new Date,
		fxjpy = 0.0,
		pricedirty = 0.0,
		priceclean = 0.0,
		pricedirty_jpy = 0.0,
		priceclean_jpy = 0.0,
		pricetype = null,
		created = new Date
      )
      
   override def toString:String = List(
		"ID:\t" + id,
		"BondID:\t" + bondid,
		"CurrencyID:\t" + currencyid,
		"PARAMDATE:\t" + paramdate,
		"FXJPY:\t" + fxjpy,
		"PRICEDIRTY:\t" + pricedirty,
		"PRICECLEAN:\t" + priceclean,
		"PRICEDIRTY_JPY:\t" + pricedirty_jpy,
		"PRICECLEAN_JPY:\t" + priceclean_jpy,
		"PRICETYPE:\t" + pricetype,
		"Created:\t" + created
		).mkString("\n")
      
}

object HistoricalPrice {
  
  val errorKeys = Set("NOPRICE")
  val maturedKeys = Set("MATURED")
  val notIssuedKeys = Set("PREISSUE")
  val noPriceKeys = errorKeys ++ maturedKeys ++ notIssuedKeys

}
