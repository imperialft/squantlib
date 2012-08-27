package squantlib.database.objectconstructor

import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate, Frequency }
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.daycounters.Thirty360
import org.jquantlib.instruments.{Bond => QLBond}
import squantlib.database.schemadefinitions.{BondPrice => dbBondPrice}
import org.jquantlib.termstructures.{YieldTermStructure, Compounding}
import org.jquantlib.cashflow.CashFlows
import java.util.{Date => javaDate}
import squantlib.model.discountcurve.DiscountCurveFactory


object BondPrice {
	 
	def build(bond:QLBond, factory:DiscountCurveFactory):dbBondPrice = 
	  build(bond, factory.valuedate, factory.fx(bond.currency.code, "JPY"), factory.paramset, factory.getyieldtermstructure(bond))

  	def build(bond:QLBond, valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure = null):dbBondPrice = {
		if (bond == null) return null
		
		val currenttime = java.util.Calendar.getInstance.getTime
	    val stddaycount = new Thirty360
	    val pricefrom = valuedate.add(30)
	    
		var expired = bond.maturityDate le bond.valuedate
		var msg:String = if (expired) "expired (" + bond.maturityDate.shortDate + "<=" + valuedate.shortDate + ")" else null
		val price = if (expired) Double.NaN else try bond.dirtyPrice catch { case e => {msg = e.getMessage; Double.NaN} }
		val toofarfromissue = if (bond.issueDate gt pricefrom) { msg = "too far from issue (" + bond.issueDate.shortDate + ">=" + pricefrom.shortDate + ")"; true} else false
		
		if (price.isNaN || price.isInfinite || fx.isNaN || toofarfromissue) 
			new dbBondPrice(
				id = bond.bondid + ":" + paramset + ":" + bond.currency.code,
				bondid = bond.bondid,
				currencyid = bond.currency.code,
				underlyingid = bond.bondid,
				comment = msg,
				paramset = paramset,
				paramdate = valuedate.longDate,
				fxjpy = fx,
				pricedirty = Double.NaN,
				priceclean = null,
				accrued = Some(Double.NaN),
				pricedirty_jpy = null,
				priceclean_jpy = null,
				accrued_jpy = null,
				yield_continuous = null,
				yield_annual = null,
				yield_semiannual = null,
				yield_simple = null,
				instrument = "BONDPRICE",
				bpvalue = null,
				atmrate = null,
				irr = null,
				currentrate = null,
				nextamount = null,
				nextdate = null,
				dur_simple = null,
				dur_modified = null,
				dur_macauley = null,
				yieldvaluebp = null,
				convexity = null, 		
				created = Some(currenttime),
				lastmodified = Some(currenttime)
		      )
		
		else {
			msg = ""
			  
			def validvalue(f:Unit => Double) = {
				val testvalue:Double = try f() catch {case e => {msg += e.getMessage; Double.NaN}}
				if (testvalue.isNaN || testvalue.isInfinite) null else Some(testvalue)
			}

			val yield_continuous = validvalue(_ => bond.`yield`(stddaycount, Compounding.Continuous, Frequency.NoFrequency))
			val yield_annual = validvalue(_ => bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Annual))
			val yield_semiann = validvalue(_ => bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Semiannual))
			val yield_simple = validvalue(_ => bond.`yield`(stddaycount, Compounding.None, Frequency.Annual))
			val price_accrued = validvalue(_ => bond.accruedAmount)
			val price_clean = validvalue(_ => bond.cleanPrice)
			
			var bps:Option[Double] = null
			var atmrate:Option[Double] = null
			var simpleduration:Option[Double] = null
			var modifiedduration:Option[Double] = null
			var macauleyduration:Option[Double] = null
			var yieldvaluebp:Option[Double] = null
			var convexity:Option[Double] = null
			
			val cfmodel = CashFlows.getInstance
			val cashflows = bond.cashflows
			val irr = validvalue(_ => cfmodel.irr(cashflows, price, new Thirty360, Compounding.Continuous, Frequency.NoFrequency, valuedate, 0.001, 1000, 0.01))
			val nextrate = validvalue(_ => cfmodel.nextCouponRate(cashflows, valuedate))
			val nextamount = validvalue(_ => cfmodel.nextCashFlows(cashflows, valuedate).map(c => c.amount).sum)
			val nextdate = Some(cfmodel.nextCashFlow(cashflows, valuedate).date.longDate)
		
			if (termstructure != null)
			{
				bps = validvalue(_ => cfmodel.bps(cashflows, termstructure, valuedate))
				atmrate = validvalue(_ => cfmodel.atmRate(cashflows, termstructure, valuedate, valuedate, 0, 0))
				
				val interestrate = termstructure.forwardRate(valuedate, bond.maturityDate, termstructure.dayCounter, Compounding.Compounded, Frequency.Semiannual)
				simpleduration = validvalue(_ => cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Simple, valuedate))
				modifiedduration = validvalue(_ => cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Modified, valuedate))
				macauleyduration = validvalue(_ => cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Macaulay, valuedate))
				yieldvaluebp = validvalue(_ => cfmodel.yieldValueBasisPoint(cashflows, interestrate, valuedate))
				convexity = validvalue(_ => cfmodel.convexity(cashflows, interestrate, valuedate))
			}
			
			val initialfx = bond.initialFX
			
			val pricedirty_jpy = if (bond.issueDate ge valuedate) Some(price)
			  					 else if (initialfx > 0) Some(price * fx / initialfx) 
								 else null
								 
			val priceclean_jpy = if (price_clean != null && (bond.issueDate ge valuedate)) Some(price_clean.get)
			  					 else if (price_clean != null && initialfx > 0) Some(price_clean.get * fx / initialfx) 
			  					 else null
			  					 
			val accrued_jpy = if (price_accrued != null && initialfx > 0) Some(price_accrued.get * fx / initialfx) else null
			
			new dbBondPrice(
				id = bond.bondid + ":" + paramset + ":" + bond.currency.code,
				bondid = bond.bondid,
				currencyid = bond.currency.code,
				underlyingid = bond.bondid,
				comment = if (msg.isEmpty) null else msg,
				paramset = paramset,
				paramdate = valuedate.longDate,
				fxjpy = fx,
				pricedirty = price,
				priceclean = price_clean,
				accrued = price_accrued,
				pricedirty_jpy = pricedirty_jpy,
				priceclean_jpy = priceclean_jpy,
				accrued_jpy = accrued_jpy,
				yield_continuous = yield_continuous,
				yield_annual = yield_annual,
				yield_semiannual = yield_semiann,
				yield_simple = yield_simple,
				instrument = "BONDPRICE",
				bpvalue = bps,
				atmrate = atmrate,
				irr = irr,
				currentrate = nextrate,
				nextamount = nextamount,
				nextdate = nextdate,
				dur_simple= simpleduration,
				dur_modified = modifiedduration,
				dur_macauley = macauleyduration,
				yieldvaluebp = yieldvaluebp,
				convexity = convexity,
				created = Some(currenttime),
				lastmodified = Some(currenttime)
		      )
		}
	}

}