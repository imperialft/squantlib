package squantlib.database

import squantlib.model.Market
import squantlib.model.rates._
import squantlib.database.schemadefinitions.{ Bond => dbBond, BondPrice, RateFXParameter, CDSParameter}
import squantlib.database.objectconstructor._
import squantlib.database.QLConstructors._
import squantlib.math.timeseries.SeriesAnalysis._
import squantlib.model.fx.FXparameter
import squantlib.setting.PricingConvention
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.time.{Date => qlDate, Frequency, TimeSeries}
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.termstructures.YieldTermStructure
import java.lang.{Double => JavaDouble}
import squantlib.model.{Bond => sBond}

/**
* Functions to directly access database to retrieve Jquantlib or Squantlib objects.
*/
object QLDB {
  
   /**
    * Returns discount curve factory.
    */
	def getMarket(paramset:String):Option[Market] = Market(DB.getRateFXParameters(paramset), DB.getCDSParameters(paramset))
	
	def getsBond(id:String):Option[sBond] = DB.getBonds(Set(id)).headOption match {
	  case None => None
	  case Some(bond) => sBond(bond)
	}
	
   /**
    * Returns jquantlib Bond object. 
    * 
    * @param id Identifications of the target bonds
    * @param factory Optional. Providing discount curve factory would automatically set discounting bond engine as default pricing engine (where applicable)
    */
	def getBond(id:String):Option[qlBond] = getBonds(Set(id)).headOption
	
	def getBond(id:String, factory:Market):Option[qlBond] = getBonds(Set(id), factory).headOption
	
	def getBonds:Set[qlBond] = bondsConstructor(DB.getBonds, null)
	
	def getBonds(bonds:Set[dbBond]):Set[qlBond] = bondsConstructor(bonds, null)
	
	def getBonds(id:Traversable[String]):Set[qlBond] = bondsConstructor(DB.getBonds(id), null)
	
	def getBonds(factory:Market):Set[qlBond] = bondsConstructor(DB.getBonds, factory)
	
	def getBonds(id:Traversable[String], factory:Market):Set[qlBond] = bondsConstructor(DB.getBonds(id), factory)
	
	def getBonds(bonds:Set[dbBond], factory:Market):Set[qlBond] = bondsConstructor(bonds, factory)
	
	def getBonds(ids:Traversable[String], builder:dbBond => Option[qlBond], pricingengine:qlBond => PricingEngine, valuedate:qlDate):Set[qlBond] = {
		val qlbonds:Set[qlBond] = DB.getBonds(ids).map(builder).flatMap(b => b)
		qlbonds.foreach(b => b.setPricingEngine(pricingengine(b), valuedate))
		qlbonds
	}
	
	def bondsConstructor(dbbonds:Set[dbBond], factory:Market):Set[qlBond] = 
	  dbbonds.map(b => PricingConvention.bondConstructor(b, factory)).flatMap(b => b)
	
   /**
    * Returns time series for non-FX input parameters.
    * @param fromDate A starting point of Date. Range includes this date.
    * @param toDate End point of Date. Range includes this date.
    * @param instrument Parameter type, such as "SWAP", "CASH" or "BASISSWAP"
    * @param asset Underlying asset, usually a currency
    * @param maturity Maturity of the target rate, such as "5Y" or "6M"
    */
	def getRateFXTimeSeries(fromDate:qlDate, toDate:qlDate, instrument:String, asset:String, maturity:String):TimeSeries[JavaDouble] = 
	  DB.getRateFXTimeSeries(fromDate.longDate, toDate.longDate, instrument, asset, maturity).toTimeSeries
	
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
	  DB.getCDSTimeSeries(fromDate.longDate, toDate.longDate, currencyid, issuerid, maturity).toTimeSeries
	  
}


