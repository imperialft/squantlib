package squantlib.database

import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import squantlib.util.UnderlyingParser
import squantlib.database.schemadefinitions.Equity

trait DbRepository {
  
  def latestParamDate:qlDate
  
  def getHistorical(id:String, startDate:qlDate, endDate:qlDate, assetId:String = null):Map[qlDate, Double]
  
  def getForwardPrices(assetId:String, id:String):Map[qlDate, Double]
  
  def getEquities:Set[Equity]
  
}

object DB {
  
  private var repository:Option[DbRepository] = None // set to enable db access
  
  def setRepository(db:DbRepository) = repository = Some(db)
  
  def getRepository:Option[DbRepository] = repository
  
  def latestParamDate:Option[qlDate] = repository.collect{case repo => repo.latestParamDate}
  
  def getPriceOn(id:String, vd:JavaDate):Option[(qlDate, Double)] = getPriceOn(id, new qlDate(vd))
	
  def getPriceOn(id:String, vd:qlDate):Option[(qlDate, Double)] = repository.flatMap{case repo => 
	if (vd gt repo.latestParamDate) None 
	else getHistorical(id).filterKeys(d => d le vd) match {
	      case mp if mp.isEmpty => None
	      case mp => Some(mp.maxBy(_._1))
	  }}
  
  def getLatestPrice(id:String):Option[(qlDate, Double)] = getHistorical(id) match {
    case p if p.isEmpty => None
    case p => Some(p.maxBy(_._1))
  }
	
  def getLatestPrices(ids:Set[String]):Map[String, Double] = ids.map(id => (id, getLatestPrice(id))).collect{case (a, Some(b)) => (a, b._2)} (collection.breakOut)
	
  def pastFixings(ids:Set[String], dates:List[qlDate], paramType:String = null):List[Map[String, Option[Double]]] = {
	  if (dates.isEmpty) {return List.empty}
	  val allhistory:Map[String, Map[qlDate, Double]] = ids.map(p => (p, getHistorical(p, dates.min, dates.max, paramType))) (collection.breakOut)
	  dates.map(d => ids.map(p => 
	    (p, allhistory(p).get(d))).toMap)
	}
	
  def getHistorical(id:String):Map[qlDate, Double] = getHistorical(id, null)
	  
  def getHistorical(id:String, paramType:String):Map[qlDate, Double] = getHistorical(id, null, null, paramType)
  
  def getHistorical(id:String, startDate:qlDate, endDate:qlDate, assetId:String = null):Map[qlDate, Double] = repository.collect{case repo => repo.getHistorical(id, startDate, endDate, assetId)}.getOrElse(Map.empty)
  
  def getForwardPrices(assetId:String, id:String):Map[qlDate, Double] = repository.collect{case repo => repo.getForwardPrices(assetId, id)}.getOrElse(Map.empty)
  
  def getEquities:Set[Equity] = repository.collect{case repo => repo.getEquities}.getOrElse(Set.empty)
}
