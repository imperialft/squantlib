package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, InputParameterSet, CDSParameterSet, BondPrice}
import squantlib.database.objectconstructor.BondPriceConstructor
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate, Frequency }
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.termstructures.YieldTermStructure


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
	
	def getBondPrice(bond:QLBond, valuedate:qlDate, fx:Double, paramset:String, ts:YieldTermStructure):BondPrice 
		= BondPriceConstructor.getprice(bond, valuedate, fx, paramset, ts)
	
	
}