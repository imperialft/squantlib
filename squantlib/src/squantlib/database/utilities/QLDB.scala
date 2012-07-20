package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, InputParameterSet, CDSParameterSet, BondPrice}
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate, Frequency }
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.daycounters.Thirty360
import org.jquantlib.termstructures.Compounding


object QLDB {

	def getDiscountCurveFactory(paramset:String):DiscountCurveFactory = {
	  val params:InputParameterSet = new InputParameterSet(transaction { from(DB.inputparameters)(c => where(c.paramset === paramset) select(c)).toSet })
	  val ratecurves = params.toLiborDiscountCurves(paramset)
	  val fxcurves = params.toFXDiscountCurves(paramset)
	
	  val cdscurves = {
		val cdsparams = new CDSParameterSet(transaction { from(DB.cdsparameters)(c => where(c.paramset === paramset) select(c)).toSet })
		cdsparams.toCDSCurves(paramset)
		}
	  
	  new DiscountCurveFactory(ratecurves ++ fxcurves, cdscurves, paramset)
	}
	
	def getBonds(id:List[String], builder:dbBond => QLBond, pricingengine:QLBond => PricingEngine, valuedate:qlDate):List[QLBond] = {
		val dbbonds:List[dbBond] = DB.getBonds(id)
		val qlbonds:List[QLBond] = dbbonds.map(b => builder(b)).filter(b => b != null)
		qlbonds.foreach(b => b.setPricingEngine(pricingengine(b), valuedate))
		qlbonds
	}
	
	def setBondPrice(bonds:List[QLBond], factory:DiscountCurveFactory, writeDb:Boolean = false):List[BondPrice] = {
		val currenttime = java.util.Calendar.getInstance.getTime
	    val valuedate = factory.valuedate
	    val stddaycount = new Thirty360
	    val pricefrom = valuedate.add(30)
	    val fxusdjpy = factory.curves("JPY").fx
	    
		val prices = bonds.map { b => {
			var expired = b.maturityDate.le(b.valuedate)
			var msg:String = if (expired) "expired (" + b.maturityDate.shortDate + "<=" + valuedate.shortDate + ")" else null
			val price = if (expired) Double.NaN else try b.dirtyPrice catch { case e => {msg = e.getMessage; Double.NaN} }
			val toofarfromissue = if (b.issueDate.gt(pricefrom)) { msg = "too far from issue (" + b.issueDate.shortDate + ">=" + pricefrom.shortDate + ")"; true} else false
			
			val fx = {
			    val fxxxxjpy = factory.curves(b.currency.code).fx
			    if (fxusdjpy == 0.0 || fxxxxjpy == 0.0) Double.NaN else fxusdjpy / fxxxxjpy
			}
			
			if (price.isNaN || price.isInfinite || fx.isNaN || toofarfromissue) {
				new BondPrice(
					id = b.bondid + ":" + factory.paramset + ":" + b.currency.code,
					bondid = b.bondid,
					currencyid = b.currency.code,
					underlyingid = b.bondid,
					comment = msg,
					paramset = factory.paramset,
					paramdate = factory.valuedate.longDate,
					fxjpy = fx,
					pricedirty = Double.NaN,
					priceclean = null,
					accrued = Some(Double.NaN),
					yield_continuous = null,
					yield_annual = null,
					yield_semiannual = null,
					yield_simple = null,
					currentrate = Some(0.0),
					instrument = "BONDPRICE",
					created = Some(currenttime),
					lastmodified = Some(currenttime)
			      )
			}
			
			else {
				msg = ""
				  
				def validvalue(f:QLBond => Double) = {
					val testvalue = try f(b) catch {case e => {msg += e.getMessage; Double.NaN}}
					if (testvalue.isNaN || testvalue.isInfinite) null else Some(testvalue)
				}

				val yield_continuous = validvalue(bond => bond.`yield`(stddaycount, Compounding.Continuous, Frequency.NoFrequency))
				val yield_annual = validvalue(bond => bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Annual))
				val yield_semiann = validvalue(bond => bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Semiannual))
				val yield_simple = validvalue(bond => bond.`yield`(stddaycount, Compounding.None, Frequency.Annual))
				val price_accrued = validvalue(bond => bond.accruedAmount)
				val price_clean = validvalue(bond => bond.cleanPrice)
				
				new BondPrice(
					id = b.bondid + ":" + factory.paramset + ":" + b.currency.code,
					bondid = b.bondid,
					currencyid = b.currency.code,
					underlyingid = b.bondid,
					comment = if (msg.isEmpty) null else msg,
					paramset = factory.paramset,
					paramdate = factory.valuedate.longDate,
					fxjpy = fx,
					pricedirty = price,
					priceclean = price_clean,
					accrued = price_accrued,
					yield_continuous = yield_continuous,
					yield_annual = yield_annual,
					yield_semiannual = yield_semiann,
					yield_simple = yield_simple,
					currentrate = Some(0.0),
					instrument = "BONDPRICE",
					created = Some(currenttime),
					lastmodified = Some(currenttime)
			      )
			}
		  }
		}
	    
	    if (writeDb){
			println("\nWriting to Database...")
			DB.setBondPrice(prices.filter(p => !p.pricedirty.isNaN))
	    }
	    
	    prices
	}
		
  
}