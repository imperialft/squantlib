package net.squantlib.database

import net.squantlib.util.Date
import java.util.{Date => JavaDate}
import net.squantlib.util.Date
import net.squantlib.util.UnderlyingParser
import net.squantlib.database.schemadefinitions.{Equity, Index}
import net.squantlib.math.timeseries.TimeSeries

trait DbRepository {
  
  def latestParamDate:Date
  
  def getHistorical(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries
  
  def getForwardPrices(assetId:String, id:String):TimeSeries
  
  def getEquities:Set[Equity]
  
  def getIndices:Set[Index]

  def getHolidays(id:String):Set[Date]
  
  def getCountryHolidayMapping: Map[String, String]

  def getCurrencyHolidayMapping: Map[String, String]

  val lastHoliday:Date

}

object DB {
  
  private var repository:Option[DbRepository] = None // set to enable db access
  
  def setRepository(db:DbRepository) = repository = Some(db)
  
  def getRepository:Option[DbRepository] = repository
  
  def latestParamDate:Option[Date] = repository.collect{case repo => repo.latestParamDate}
  
  def getPriceOn(id:String, vd:JavaDate):Option[(Date, Double)] = getPriceOn(id, Date(vd))
	
  def getPriceOn(id:String, vd:Date):Option[(Date, Double)] = repository.flatMap{case repo => 
	if (vd gt repo.latestParamDate) None 
	else getHistorical(id).filterKeys(d => d le vd) match {
      case mp if mp.isEmpty => None
      case mp => Some(mp.maxBy(_._1))
  }}
  
  def getLatestPrice(id:String):Option[(Date, Double)] = getHistorical(id) match {
    case p if p.isEmpty => None
    case p => Some(p.maxBy(_._1))
  }
	
  def getLatestPrices(ids:Set[String]):Map[String, Double] = ids.map(id => (id, getLatestPrice(id))).collect{case (a, Some(b)) => (a, b._2)} (collection.breakOut)
	
  def pastFixing(id:String, dates:List[Date], paramType:String = null):List[Option[Double]] = {
    if (dates.isEmpty) {return List.empty}
    val allhistory = getHistorical(id, dates.min, dates.max, paramType)
    dates.map(d => allhistory.get(d))
  }
  
  def pastFixings(ids:Set[String], dates:List[Date], paramType:String = null, fillBlank:Boolean = false):List[Map[String, Option[Double]]] = {
    if (dates.isEmpty) {return List.empty}
    val allhistory:Map[String, Map[Date, Double]] = ids.map(p => (p, getHistorical(p, dates.min.sub(30), dates.max, paramType).toMap)) (collection.breakOut)
    if (fillBlank){
      dates.map(d => ids.map(p => (p, 
        if (d gt latestParamDate.getOrElse(Date.currentDate)) None
        else {
          val minv = allhistory(p).filterKeys(v => v le d)
          if (minv.isEmpty) None else Some(minv.maxBy(_._1)._2)
        })).toMap)
    }
    else {
      dates.map(d => ids.map(p => (p, allhistory(p).get(d))).toMap)
    }
  }
	
  def getHistorical(id:String):TimeSeries = getHistorical(id, null)
	  
  def getHistorical(id:String, paramType:String):TimeSeries = getHistorical(id, null, null, paramType)
  
  def getHistorical(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries = repository match {
    case Some(repo) => repo.getHistorical(id, startDate, endDate, assetId)
    case None => TimeSeries.empty
  }
  
  def getForwardPrices(assetId:String, id:String):TimeSeries = repository.collect{case repo => repo.getForwardPrices(assetId, id)}.getOrElse(TimeSeries.empty)
  
  def getEquities:Set[Equity] = repository.collect{case repo => repo.getEquities}.getOrElse(Set.empty)
  
  def getIndices:Set[Index] = repository.collect{case repo => repo.getIndices}.getOrElse(Set.empty)
  
  def getHolidays(id:String):Set[Date] = repository.collect{case repo => repo.getHolidays(id)}.getOrElse(Set.empty)

  def getCountryHolidayMapping: Map[String, String] = repository.collect{case repo => repo.getCountryHolidayMapping}.getOrElse(Map.empty)

  def getCurrencyHolidayMapping: Map[String, String] = repository.collect{case repo => repo.getCurrencyHolidayMapping}.getOrElse(Map.empty)

  
  def lastHoliday:Date = repository.collect{case repo => repo.lastHoliday}.getOrElse(Date.currentDate)
  
}
