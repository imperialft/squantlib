package squantlib.database

import squantlib.model.Market
import squantlib.database.schemadefinitions.{ Bond => dbBond, RateFXParameter, CDSParameter}
import org.jquantlib.time.{Date => qlDate, Frequency, TimeSeries}
import java.util.{Date => JavaDate}
import squantlib.model.{Bond => sBond}
import squantlib.model.bond.BondSetting

/**
* Functions to directly access database to retrieve Jquantlib or Squantlib objects.
*/
object QLDB {
  
  var bondSetting:BondSetting = BondSetting.getDefault
  
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
  
  def getMarket(paramdate:qlDate):Option[Market] = getMarket(paramdate.longDate)
  
  def getMarket(paramdate:JavaDate):Option[Market] = {
    val pset = ("%tY%<tm%<td" format paramdate) + "-000"
    getMarket(pset)
  }
  
  def getMarket(paramset:String, valueDate:qlDate):Option[Market] = 
    Market(DB.getRateFXParameters(paramset), DB.getCDSParameters(paramset), valueDate)
	
  def getBond(id:String):Option[sBond] = DB.getBonds(Set(id)).headOption.flatMap {bond => 
	  val b = sBond(bond)
	  if (b.isDefined) bondSetting(b.get)
	  b
  }
	
  def getBonds(ids:Set[String]):Set[sBond] = DB.getBonds(ids).map(sBond(_)).collect{
	  case Some(bond) => bondSetting(bond); bond
  }
	
  def getBonds:Set[sBond] = getBonds(DB.getBonds)
	
  def getBonds(date:JavaDate):Set[sBond] = getBonds(DB.getBonds(date))
  
  def getBonds(par:Boolean, showProgress:Boolean):Set[sBond] = getBonds(DB.getBonds, par, showProgress)
	
  def getBonds(bonds:Set[dbBond], par:Boolean = false, showProgress:Boolean = false)(implicit d:DummyImplicit):Set[sBond] = {
	  println("initialize bonds..")
	  val sbonds = (if (par) bonds.par else bonds).map(b => {
	    if (showProgress) println(b.id + " : initialize") 
	    sBond(b)}).seq
	  println(sbonds.size + " bonds retrieved - initializing")
	  sbonds.collect{case Some(bond) => bondSetting(bond); bond}.toSet
  }
  
  def getBondsWithLatestMarket(par:Boolean, showProgress:Boolean):Set[sBond] = {
    val bonds = getBonds(par, showProgress)
    QLDB.getMarket match {
      case Some(mkt) => (if (par) bonds.par else bonds).foreach(b => {
        if (showProgress) println(b.id + " : assign market")
        b.market = mkt})
      case _ => println("market cannot be initialized")
    }
    bonds
  }
	  
}

