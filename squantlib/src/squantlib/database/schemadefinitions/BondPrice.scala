package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity


class BondPrice(@Column("ID")			var id:String, 
			@Column("BondID")			var bondid:String,
			@Column("CurrencyID")		var currencyid:String,
			@Column("COMMENT")			var comment:String,
			@Column("PARAMSET")			var paramset:String,
			@Column("PARAMDATE")		var paramdate:Date,
			@Column("FXJPY")			var fxjpy:Double,
			@Column("PRICEDIRTY")		var pricedirty:Double,
			@Column("PRICECLEAN")		var priceclean:Option[Double],
			@Column("ACCRUED")			var accrued:Option[Double],
			@Column("PRICEDIRTY_JPY")	var pricedirty_jpy:Option[Double],
			@Column("PRICECLEAN_JPY")	var priceclean_jpy:Option[Double],
			@Column("ACCRUED_JPY")		var accrued_jpy:Option[Double],
			@Column("YIELD_CONTINUOUS")	var yield_continuous:Option[Double],
			@Column("YIELD_ANNUAL")		var yield_annual:Option[Double],
			@Column("YIELD_SEMIANNUAL")	var yield_semiannual:Option[Double],
			@Column("YIELD_SIMPLE")		var yield_simple:Option[Double],
			@Column("BPVALUE")			var bpvalue:Option[Double],
			@Column("ATMRATE")			var atmrate:Option[Double],
			@Column("IRR")				var irr:Option[Double],
			@Column("CURRENTRATE")		var currentrate:Option[Double],
			@Column("NEXTAMOUNT")		var nextamount:Option[Double],
			@Column("NEXTDATE")			var nextdate:Option[Date],
			@Column("DUR_SIMPLE")		var dur_simple:Option[Double],
			@Column("DUR_MODIFIED")		var dur_modified:Option[Double],
			@Column("DUR_MACAULEY")		var dur_macauley:Option[Double],
			@Column("YIELDVALUE")		var yieldvaluebp:Option[Double],
			@Column("CONVEXITY")		var convexity:Option[Double], 
			@Column("REMAININGLIFE")	var remaininglife:Option[Double], 
			@Column("Created")			var created:Option[Date],
			@Column("LastModified")		var lastmodified:Option[Date]
			) extends KeyedEntity[String]{
  
  def this() = this(
		id = null,
		bondid = null,
		currencyid = null,
		comment = null,
		paramset = null,
		paramdate = new Date,
		fxjpy = 0.0,
		pricedirty = 0.0,
		priceclean = Some(-999.0),
		accrued = Some(-999.0),
		pricedirty_jpy = Some(-999.0),
		priceclean_jpy = Some(-999.0),
		accrued_jpy = Some(-999.0),
		yield_continuous = Some(-999.0),
		yield_annual = Some(-999.0),
		yield_semiannual = Some(-999.0),
		yield_simple = Some(-999.0),
		bpvalue = Some(-999.0),
		atmrate = Some(-999.0),
		irr = Some(-999.0),
		currentrate = Some(-999.0),
		nextamount = Some(-999.0),
		nextdate = None,
		dur_simple = Some(-999.0),
		dur_modified= Some(-999.0),
		dur_macauley= Some(-999.0),
		yieldvaluebp = Some(-999.0),
		convexity = Some(-999.0), 		
		remaininglife = Some(999.0), 		
		created = None,
		lastmodified = None
      )
      
  override def toString():String = format("%-15s %-5s %-5s %-10s %-5s", id, currencyid, pricedirty, paramset, if (accrued.isEmpty) "" else accrued.get)
      
}