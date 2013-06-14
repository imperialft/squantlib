package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class LatestPrice(@Column("ID")			override var id:String, 
			@Column("BondID")			var bondid:String,
			@Column("CurrencyID")		var currencyid:String,
			@Column("COMMENT")			var comment:String,
			@Column("PARAMSET")			var paramset:String,
			@Column("PARAMDATE")		var paramdate:Date,
			@Column("FXJPY")			var fxjpy:Double,
			@Column("PRICEDIRTY")		var pricedirty:Double,
			@Column("PRICECLEAN")		var priceclean:Double,
			@Column("ACCRUED")			var accrued:Double,
			@Column("PRICEDIRTY_JPY")	var pricedirty_jpy:Double,
			@Column("PRICECLEAN_JPY")	var priceclean_jpy:Double,
			@Column("ACCRUED_JPY")		var accrued_jpy:Double,
			@Column("YIELD_CONTINUOUS")	var yield_continuous:Option[Double],
			@Column("YIELD_ANNUAL")		var yield_annual:Option[Double],
			@Column("YIELD_SEMIANNUAL")	var yield_semiannual:Option[Double],
			@Column("YIELD_SIMPLE")		var yield_simple:Option[Double],
			@Column("BPVALUE")			var bpvalue:Option[Double],
			@Column("IRR")				var irr:Option[Double],
			@Column("CURRENTRATE")		var currentrate:Option[Double],
			@Column("NEXTAMOUNT")		var nextamount:Option[Double],
			@Column("NEXTDATE")			var nextdate:Option[Date],
			@Column("DUR_SIMPLE")		var dur_simple:Option[Double],
			@Column("DUR_MODIFIED")		var dur_modified:Option[Double],
			@Column("DUR_MACAULEY")		var dur_macauley:Option[Double],
			@Column("YIELDVALUE")		var yieldvaluebp:Option[Double],
			@Column("CONVEXITY")		var convexity:Option[Double], 
			@Column("REMAININGLIFE")	var remaininglife:Double, 
			@Column("PARMTMYIELD")		var parMtMYield:Option[Double], 
			@Column("PARMTMFX")			var parMtMfx:Option[Double], 
			@Column("RATEDELTA")		var rateDelta:String, 
			@Column("RATEVEGA")			var rateVega:String, 
			@Column("FXDELTA")			var fxDelta:String, 
			@Column("FXDELTAJPY")		var fxDeltaJpy:String, 
			@Column("FXVEGA")			var fxVega:String, 
			@Column("PRICETYPE")		var pricetype:String, 
			@Column("VOLATILITY")		var volatility:Double, 
			@Column("Created")			var created:Date,
			@Column("LastModified")		var lastmodified:Date
			) extends StringEntity {
  
  def getFieldMap:Map[String, Any] = getObjectFieldMap(this)
  
  def isPriced = !(LatestPrice.noPriceKeys contains pricetype)
  
  def isMatured = LatestPrice.maturedKeys contains pricetype
  
  def isError = LatestPrice.errorKeys contains pricetype
  
  def isNotIssued = LatestPrice.notIssuedKeys contains pricetype
    
  def this() = this(
		id = null,
		bondid = null,
		currencyid = null,
		comment = null,
		paramset = null,
		paramdate = new Date,
		fxjpy = 0.0,
		pricedirty = 0.0,
		priceclean = 0.0,
		accrued = 0.0,
		pricedirty_jpy = -999.0,
		priceclean_jpy = -999.0,
		accrued_jpy = -999.0,
		yield_continuous = Some(-999.0),
		yield_annual = Some(-999.0),
		yield_semiannual = Some(-999.0),
		yield_simple = Some(-999.0),
		bpvalue = Some(-999.0),
		irr = Some(-999.0),
		currentrate = Some(-999.0),
		nextamount = Some(-999.0),
		nextdate = None,
		dur_simple = Some(-999.0),
		dur_modified= Some(-999.0),
		dur_macauley= Some(-999.0),
		yieldvaluebp = Some(-999.0),
		convexity = Some(-999.0), 		
		remaininglife = -999.0, 		
		parMtMYield= Some(-999.0), 
		parMtMfx = Some(-999.0), 
		rateDelta = null, 
		rateVega = null,
		fxDelta = null, 
		fxDeltaJpy = null, 
		fxVega = null,
		pricetype = null,
		volatility = 0.1,
		created = null,
		lastmodified = null
      )
      
   override def toString:String = List(
		"ID:\t" + id,
		"BondID:\t" + bondid,
		"CurrencyID:\t" + currencyid,
		"COMMENT:\t" + comment,
		"PARAMSET:\t" + paramset,
		"PARAMDATE:\t" + paramdate,
		"FXJPY:\t" + fxjpy,
		"PRICEDIRTY:\t" + pricedirty,
		"PRICECLEAN:\t" + priceclean,
		"ACCRUED:\t" + accrued,
		"PRICEDIRTY_JPY:\t" + pricedirty_jpy,
		"PRICECLEAN_JPY:\t" + priceclean_jpy,
		"ACCRUED_JPY:\t" + accrued_jpy,
		"YIELD_CONTINUOUS:\t" + yield_continuous.getOrElse("None"),
		"YIELD_ANNUAL:\t" + yield_annual.getOrElse("None"),
		"YIELD_SEMIANNUAL:\t" + yield_semiannual.getOrElse("None"),
		"YIELD_SIMPLE:\t" + yield_simple.getOrElse("None"),
		"BPVALUE:\t" + bpvalue.getOrElse("None"),
		"IRR:\t" + irr.getOrElse("None"),
		"CURRENTRATE:\t" + currentrate.getOrElse("None"),
		"NEXTAMOUNT:\t" + nextamount.getOrElse("None"),
		"NEXTDATE:\t" + nextdate.getOrElse("None"),
		"DUR_SIMPLE:\t" + dur_simple.getOrElse("None"),
		"DUR_MODIFIED:\t" + dur_modified.getOrElse("None"),
		"DUR_MACAULEY:\t" + dur_macauley.getOrElse("None"),
		"YIELDVALUE:\t" + yieldvaluebp.getOrElse("None"),
		"CONVEXITY:\t" + convexity.getOrElse("None"),
		"REMAININGLIFE:\t" + remaininglife,
		"PARMTMYIELD:\t" + parMtMYield.getOrElse("None"),
		"PARMTMFX:\t" + parMtMfx.getOrElse("None"),
		"RATEDELTA:\t" + rateDelta,
		"RATEVEGA:\t" + rateVega,
		"FXDELTA:\t" + fxDelta,
		"FXDELTAJPY:\t" + fxDeltaJpy,
		"FXVEGA:\t" + fxVega,
		"PRICETYPE:\t" + pricetype,
		"VOLATILITY:\t" + volatility,
		"Created:\t" + created,
		"LastModified:\t" + lastmodified
		).mkString("\n")
		
  def getHistoricalPrice:HistoricalPrice = {
    new HistoricalPrice(
	    id = id + ":" + ("%tY%<tm%<td" format paramdate),
		bondid = id,
		paramdate = paramdate,
		currencyid = currencyid,
		fxjpy = fxjpy,
		pricedirty = pricedirty,
		priceclean = priceclean,
		pricedirty_jpy = pricedirty_jpy,
		priceclean_jpy = priceclean_jpy,
		pricetype = pricetype,
		created = created)
  }
  
      
}

object LatestPrice {
  
  val errorKeys = Set("NOPRICE")
  val maturedKeys = Set("MATURED")
  val notIssuedKeys = Set("PREISSUE")
  val noPriceKeys = errorKeys ++ maturedKeys ++ notIssuedKeys

}




//}