package squantlib.database.utilities

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, InputParameterSet, CDSParameterSet}
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.Bond


object QuickConstructor {

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
	
	def getDBBonds:Map[String, dbBond] = {
	  val dbresult = transaction { from(DB.bonds)(c => select(c)).toSet }
	  dbresult.map(b => (b.id, b)).toMap
	}
	
	def getBonds(targetbonds:Set[String], valuedate:Date, builder:dbBond => Bond):Map[String, Bond] = {
		val vdlong = valuedate.longDate
		val dbbonds:Set[squantlib.database.schemadefinitions.Bond] = transaction { from(DB.bonds)(c => where(c.id in targetbonds and c.maturity > vdlong) select(c)).toSet }
		dbbonds.map(b => (b.id, builder(b))).toMap;
	}

  
}