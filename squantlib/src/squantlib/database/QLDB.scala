package squantlib.database

import squantlib.model.Market
import squantlib.model.rates._
import squantlib.database.schemadefinitions.{ Bond => dbBond, BondPrice, RateFXParameter, CDSParameter}
import squantlib.database.QLConstructors._
import squantlib.math.timeseries.SeriesAnalysis._
import squantlib.model.fx.FXInitializer
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.time.{Date => qlDate, Frequency, TimeSeries}
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.termstructures.YieldTermStructure
import java.lang.{Double => JavaDouble}
import java.util.{Date => JavaDate}
import squantlib.model.{Bond => sBond}
import squantlib.setting.DefaultBondSetting

/**
* Functions to directly access database to retrieve Jquantlib or Squantlib objects.
*/
object QLDB {
  
   /**
    * Returns discount curve factory.
    */
  
	def getMarket:Option[Market] = getMarket(DB.getLatestParamSet._1)
  
	def getMarket(paramset:String):Option[Market] = Market(DB.getRateFXParameters(paramset), DB.getCDSParameters(paramset))
	
	def getMarket(paramset:String, valueDate:qlDate):Option[Market] = Market(DB.getRateFXParameters(paramset), DB.getCDSParameters(paramset), valueDate)
	
	def getLatestMarket:Option[Market] = getMarket(DB.getLatestParamSet._1)
	
	def getBond(id:String):Option[sBond] = DB.getBonds(Set(id)).headOption.flatMap {bond => 
	  val b = sBond(bond)
	  if (b.isDefined) DefaultBondSetting(b.get)
	  b
	}
	
	def getBonds(ids:Set[String]):Set[sBond] = DB.getBonds(ids).map(sBond(_)).collect{
	  case Some(bond) => DefaultBondSetting(bond); bond
	}
	
	def getBonds:Set[sBond] = getBonds(DB.getBonds)
	
	def getBonds(date:JavaDate):Set[sBond] = getBonds(DB.getBonds(date))
	
	def getBonds(bonds:Set[dbBond], par:Boolean = false)(implicit d:DummyImplicit):Set[sBond] = {
	  println("retrieve bonds..")
	  val sbonds = (if (par) bonds.par else bonds).map(b => sBond(b)).seq
	  sbonds.collect{case Some(bond) => DefaultBondSetting(bond); bond}.toSet
	}
	
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

