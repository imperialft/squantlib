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
	 
	def apply(bond:QLBond, factory:DiscountCurveFactory):dbBondPrice = build(bond, factory)
  	def apply(bond:QLBond, valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure = null):dbBondPrice = build(bond, valuedate, fx, paramset, termstructure:YieldTermStructure)
	
	def build(bond:QLBond, factory:DiscountCurveFactory):dbBondPrice = 
	  build(bond, factory.valuedate, factory.fx(bond.currency.code, "JPY"), factory.paramset, factory.getyieldtermstructure(bond).orNull)

  	def build(bond:QLBond, valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure = null):dbBondPrice = {
		if (bond == null) return null
		
		val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	    val stddaycount = new Thirty360
	    val pricefrom = valuedate.add(265)
	    
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
				priceclean = None,
				accrued = Some(Double.NaN),
				pricedirty_jpy = None,
				priceclean_jpy = None,
				accrued_jpy = None,
				yield_continuous = None,
				yield_annual = None,
				yield_semiannual = None,
				yield_simple = None,
				instrument = "BONDPRICE",
				bpvalue = None,
				atmrate = None,
				irr = None,
				currentrate = None,
				nextamount = None,
				nextdate = None,
				dur_simple = None,
				dur_modified = None,
				dur_macauley = None,
				yieldvaluebp = None,
				convexity = None, 		
				remaininglife = None, 		
				created = Some(currenttime),
				lastmodified = Some(currenttime)
		      )
		
		else {
			msg = ""
			  
			def validvalue(f: => Double):Option[Double] = {
				val testvalue:Double = try f catch {case e => {msg += e.getMessage; Double.NaN}}
				if (testvalue.isNaN || testvalue.isInfinite) None else Some(testvalue)
			}

			val yield_continuous = validvalue(bond.`yield`(stddaycount, Compounding.Continuous, Frequency.NoFrequency))
			val yield_annual = validvalue(bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Annual))
			val yield_semiann = validvalue(bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Semiannual))
			val yield_simple = validvalue(bond.`yield`(stddaycount, Compounding.None, Frequency.Annual))
			val price_accrued = validvalue(bond.accruedAmount)
			val price_clean = validvalue(bond.cleanPrice)
			
			var bps:Option[Double] = None
			var atmrate:Option[Double] = None
			var simpleduration:Option[Double] = None
			var modifiedduration:Option[Double] = None
			var macauleyduration:Option[Double] = None
			var yieldvaluebp:Option[Double] = None
			var convexity:Option[Double] = None
			
			val cfmodel = CashFlows.getInstance
			val cashflows = bond.cashflows
			val irr = validvalue(cfmodel.irr(cashflows, price, new Thirty360, Compounding.Continuous, Frequency.NoFrequency, valuedate, 0.001, 1000, 0.01))
			val nextrate = validvalue(cfmodel.nextCouponRate(cashflows, valuedate))
			val nextamount = validvalue(cfmodel.nextCashFlows(cashflows, valuedate).map(c => c.amount).sum)
			val nextdate = Some(cfmodel.nextCashFlow(cashflows, valuedate).date.longDate)
			val remaininglife = validvalue(bond.remainingLife)
		
			if (termstructure != null)
			{
				bps = validvalue(cfmodel.bps(cashflows, termstructure, valuedate))
				atmrate = validvalue(cfmodel.atmRate(cashflows, termstructure, valuedate, valuedate, 0, 0))
				
				val interestrate = termstructure.forwardRate(valuedate, bond.maturityDate, termstructure.dayCounter, Compounding.Compounded, Frequency.Semiannual)
				simpleduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Simple, valuedate))
				modifiedduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Modified, valuedate))
				macauleyduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Macaulay, valuedate))
				yieldvaluebp = validvalue(cfmodel.yieldValueBasisPoint(cashflows, interestrate, valuedate))
				convexity = validvalue(cfmodel.convexity(cashflows, interestrate, valuedate))
			}
			
			val initialfx = bond.initialFX
			
			val pricedirty_jpy = if (bond.issueDate ge valuedate) None
			  					 else if (initialfx > 0) Some(price * fx / initialfx) 
								 else None
								 
			val priceclean_jpy = if (bond.issueDate ge valuedate) None
			  					 else if (price_clean.isDefined && initialfx > 0) Some(price_clean.get * fx / initialfx) 
			  					 else None
			  					 
			val accrued_jpy = if (price_accrued.isDefined && initialfx > 0) Some(price_accrued.get * fx / initialfx) else None
			
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
				remaininglife = remaininglife,
				created = Some(currenttime),
				lastmodified = Some(currenttime)
		      )
		}
	}

}