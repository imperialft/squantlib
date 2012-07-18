package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, InputParameterSet, CDSParameterSet}
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate}
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.pricingengines.PricingEngine


object DBConstructor {

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
	
	
	def getQLBonds(id:List[String], builder:dbBond => QLBond, pricingengine:QLBond => PricingEngine, valuedate:qlDate):Map[String, QLBond] = {
		val dbbonds:List[dbBond] = DB.getBonds(id)
		val qlbonds:Map[String, QLBond] = dbbonds.map(b => (b.id, builder(b))).toMap;
		qlbonds.foreach(b => if (b._2 != null) b._2.setPricingEngine(pricingengine(b._2), valuedate))
		qlbonds
	}

  
}