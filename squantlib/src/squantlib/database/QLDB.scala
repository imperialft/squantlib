package squantlib.database

import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database.schemadefinitions.{ Bond => dbBond, BondPrice, InputParameter}
import squantlib.database.objectconstructor._
import squantlib.database.QLConstructors._
import squantlib.model.timeseries.TsAnalysis._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.time.{Date => qlDate, Frequency, TimeSeries}
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.termstructures.YieldTermStructure
import java.lang.{Double => JavaDouble}

/**
* Functions to directly access database to retrieve Jquantlib or Squantlib objects.
*/
object QLDB {
  
   /**
    * Returns discount curve factory.
    */
	def getDiscountCurveFactory(paramset:String):DiscountCurveFactory = {
	  val discountcurves = DB.getInputParameters(paramset).toDiscountCurves 
	  val cdscurves = DB.getCDSParameters(paramset).toCDSCurves
	  
	  new DiscountCurveFactory(
	    discountcurves.map(c => (c.currency.code, c)).toMap, 
	    cdscurves.map(c => (c.issuerid, c)).toMap, 
	    paramset)
	}
	
   /**
    * Returns jquantlib Bond object. 
    * 
    * @param id Identifications of the target bonds
    * @param factory Optional. Providing discount curve factory would automatically set discounting bond engine as default pricing engine (where applicable)
    */
	def getBonds(id:Traversable[String], factory:DiscountCurveFactory = null):List[QLBond] = bondConstructor(DB.getBonds(id))
	def getBonds(factory:DiscountCurveFactory):List[QLBond] = bondConstructor(DB.getAllBonds, factory)
	def getBonds:List[QLBond] = bondConstructor(DB.getAllBonds)
	
	def getBonds(id:Traversable[String], builder:dbBond => QLBond, pricingengine:QLBond => PricingEngine, valuedate:qlDate):List[QLBond] = {
		val dbbonds:List[dbBond] = DB.getBonds(id)
		val qlbonds:List[QLBond] = dbbonds.map(b => builder(b)).filter(b => b != null)
		qlbonds.foreach(b => b.setPricingEngine(pricingengine(b), valuedate))
		qlbonds
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
	 
   /**
    * Returns time series for non-FX input parameters.
    * @param fromDate A starting point of Date. Range includes this date.
    * @param toDate End point of Date. Range includes this date.
    * @param instrument Parameter type, such as "SWAP", "CASH" or "BASISSWAP"
    * @param asset Underlying asset, usually a currency
    * @param maturity Maturity of the target rate, such as "5Y" or "6M"
    */
	def getTimeSeries(fromDate:qlDate, toDate:qlDate, instrument:String, asset:String, maturity:String):TimeSeries[JavaDouble] = 
	  DB.getTimeSeries(fromDate.longDate, toDate.longDate, instrument, asset, maturity).toTimeSeries
	
   /**
    * Returns time series for FX parameters.
    * @param fromDate A starting point of Date. Range includes this date.
    * @param toDate End point of Date. Range includes this date.
    * @param fx1 Currency to be measured
    * @param fx2 Measuring currency.
    * 		 For example, fx1 = "USD", fx2 = "JPY" would provide number of JPY per 1 USD = around 80.00
    */
	def getFXTimeSeries(fromDate:qlDate, toDate:qlDate, fx1:String, fx2:String):TimeSeries[JavaDouble] = 
	  DB.getFXTimeSeries(fromDate.longDate, toDate.longDate, fx1, fx2).toTimeSeries
	  
   /**
    * Returns time series for CDS parameters.
    * @param fromDate A starting point of Date. Range includes this date.
    * @param toDate End point of Date. Range includes this date.
    * @param currencyid Currency of the CDS quotation
    * @param issuer Target issuer
    * @param maturity CDS maturity
    */
	def getCDSTimeSeries(fromDate:qlDate, toDate:qlDate, currencyid:String, issuerid:String, maturity:String):TimeSeries[JavaDouble] = 
	  DB.getTimeSeries(fromDate.longDate, toDate.longDate, currencyid, issuerid, maturity).toTimeSeries
	  
}


