package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database.DB
import squantlib.database.schemadefinitions.{ Bond => dbBond, BondPrice, InputParameter}
import squantlib.database.objectconstructor._
import squantlib.database.QLConstructors._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate, Frequency }
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.termstructures.YieldTermStructure


object QLDB {
  
	def getDiscountCurveFactory(paramset:String):DiscountCurveFactory = {
	  val discountcurves = DB.getInputParameters(paramset).toDiscountCurves 
	  val cdscurves = DB.getCDSParameters(paramset).toCDSCurves
	  
	  new DiscountCurveFactory(
	    discountcurves.map(c => (c.currency.code, c)).toMap, 
	    cdscurves.map(c => (c.issuerid, c)).toMap, 
	    paramset)
	}
	
	private def bondConstructor(dbbonds:List[dbBond], factory:DiscountCurveFactory = null):List[QLBond] = {
		dbbonds.map { b =>
		  b.productid match {
		    
		    case "SB" | "STEPUP" | "DISC" =>
		      val bond = b.toFixedRateBond
		      if (bond != null && factory != null) bond.setPricingEngine(factory.getdiscountbondengine(bond), factory.valuedate)
		      bond
		      
		    case _ =>
		      null
		  }
		}.filter(b => b != null)
	}
	
	def getBonds(id:Traversable[String], factory:DiscountCurveFactory = null):List[QLBond] = bondConstructor(DB.getBonds(id))
	def getBonds(factory:DiscountCurveFactory):List[QLBond] = bondConstructor(DB.getAllBonds, factory)
	def getBonds:List[QLBond] = bondConstructor(DB.getAllBonds)
	
	def getBonds(id:Traversable[String], builder:dbBond => QLBond, pricingengine:QLBond => PricingEngine, valuedate:qlDate):List[QLBond] = {
		val dbbonds:List[dbBond] = DB.getBonds(id)
		val qlbonds:List[QLBond] = dbbonds.map(b => builder(b)).filter(b => b != null)
		qlbonds.foreach(b => b.setPricingEngine(pricingengine(b), valuedate))
		qlbonds
	}
	
	
}


