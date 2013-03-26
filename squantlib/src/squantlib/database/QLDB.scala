package squantlib.database

import squantlib.model.Market
//import squantlib.model.rates._
import squantlib.database.schemadefinitions.{ Bond => dbBond, BondPrice, RateFXParameter, CDSParameter}
//import squantlib.database.QLConstructors._
//import squantlib.math.timeseries.SeriesAnalysis._
//import squantlib.model.fx.FXInitializer
//import org.squeryl.PrimitiveTypeMode._
//import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.time.{Date => qlDate, Frequency, TimeSeries}
//import org.jquantlib.pricingengines.PricingEngine
//import org.jquantlib.termstructures.YieldTermStructure
//import java.lang.{Double => JavaDouble}
import java.util.{Date => JavaDate}
import squantlib.model.{Bond => sBond}
import squantlib.setting.DefaultBondSetting

/**
* Functions to directly access database to retrieve Jquantlib or Squantlib objects.
*/
object QLDB {
  
	def getMarket:Option[Market] = {
	  val paramset = DB.getLatestParamSet._1
	  println("initialize market with paramset " + paramset)
	  getMarket(paramset)
	}
  
	def getMarket(paramset:String):Option[Market] = {
	  println("access db for market param")
	  val ratefx = DB.getRateFXParameters(paramset)
	  val cds = DB.getCDSParameters(paramset)
	  println("initialize market with " + ratefx.size + " ratefx params & " + cds.size + " cds params")
	  Market(ratefx, cds)
	}
	
	def getMarket(paramset:String, valueDate:qlDate):Option[Market] = Market(DB.getRateFXParameters(paramset), DB.getCDSParameters(paramset), valueDate)
	
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
	  println("initialize bonds..")
	  val sbonds = (if (par) bonds.par else bonds).map(b => sBond(b)).seq
	  println(sbonds.size + " bonds retrieved - initializing")
	  sbonds.collect{case Some(bond) => DefaultBondSetting(bond); bond}.toSet
	}
	  
}

