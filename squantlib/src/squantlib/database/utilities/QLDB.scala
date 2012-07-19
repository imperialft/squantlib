package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, InputParameterSet, CDSParameterSet, BondPrice}
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate}
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.pricingengines.PricingEngine


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
//	    var errorlist = scala.collection.mutable.Map.empty[String, String]
		val currenttime = java.util.Calendar.getInstance.getTime
	    
		val prices = bonds.map { b => {
			var errormsg:String = null
			val price = try { b.dirtyPrice } catch { case e => {errormsg = e.getMessage; Double.NaN} }
			new BondPrice(
				id = b.bondid + ":" + factory.paramset + ":" + b.currency.code,
				bondid = b.bondid,
				currencyid = b.currency.code,
				underlyingid = b.bondid,
				comment = errormsg,
				paramset = factory.paramset,
				paramdate = factory.valuedate.longDate,
				fxjpy = factory.curves(b.currency.code).fx,
				pricedirty = price,
				created = Some(currenttime),
				lastmodified = Some(currenttime),
				accrued = Some(0.0),
				currentrate = Some(0.0),
				instrument = "BONDPRICE"
		      )
		}}
	    
	    if (writeDb){
			println("\nWriting to Database...")
			DB.setBondPrice(prices.filter(p => !p.pricedirty.isNaN))
	    }
	    
	    prices
	}
		
  
}