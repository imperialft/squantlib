package net.squantlib.database

import net.squantlib.util.{Date, FixingInformation, UnderlyingFixingInfo, UnderlyingParser}
import net.squantlib.database.schemadefinitions.{Equity, Index}
import net.squantlib.math.timeseries.TimeSeries
import java.util.{Date => JavaDate}

trait DbRepository {
  
  def latestParamDate:Date
  
  def getHistorical(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries

  def getHistoricalLow(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries

  def getHistoricalHigh(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries
  
  def getForwardPrices(assetId:String, id:String):TimeSeries
  
  def getEquities:Set[Equity]
  
  def getIndices:Set[Index]

  def getHolidays(id:String):Set[Date]

  def getCountryIds:Set[String]

  def getCurrencyIds:Set[String]
  
//  def getCountryHolidayMapping: Map[String, String]
//
//  def getCurrencyHolidayMapping: Map[String, String]

//  val lastHoliday:Date

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

  def pastFixings(ids:Set[String], dates:List[Date]):List[Map[String, Option[Double]]] = pastFixings(ids, dates, null)

  def pastFixings(ids:Set[String], dates:List[Date], paramType:String):List[Map[String, Option[Double]]] = pastFixings(ids, dates, paramType, false)

  def pastFixings(
    ids:Set[String],
    dates:List[Date],
    paramType:String,
    fillBlank:Boolean):List[Map[String, Option[Double]]] = {

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

  def getFixings(
    ids: Set[String],
    dates: List[Date],
    fixingInfo: FixingInformation
  ):List[Map[String, Double]] = {

    val underlyingFixingInfos:Map[String, UnderlyingFixingInfo] = ids.map(ul =>
      (ul, fixingInfo.getUnderlyingFixing(ul))
    ).filter{case (ul, infos) =>
      !infos.fixingPages.isEmpty
    }.toMap

    val allFixingPages:Set[String] = ids ++ underlyingFixingInfos.map{case (ul, infos) => infos.fixingPages.map(p => p.pageList).flatten}.flatten.toSet

    val allPastFixings:List[Map[String, Option[Double]]] = pastFixings(allFixingPages, dates)

    val baseFixings:List[Map[String, Double]] = allPastFixings.map(_.collect{case (k, Some(v)) => (k, v)})

    val customFixings:List[Map[String, Double]] = baseFixings.map(fixingMap => {
      underlyingFixingInfos.map{case (ul, infos) => (ul, infos.getPriceFromFixings(fixingMap))}.collect{case (ul, Some(v)) => (ul, v)}
    })

    baseFixings.zip(customFixings).map{case (b, c) => b ++ c}

//    allPastFixings.zip(customFixings).map{case (bMap, cMap) =>
//      bMap.map{case (ul, v) =>
//        cMap.get(ul) match {
//          case Some(vv) => (ul -> vv)
//          case None if !ul.contains(":") => (ul -> v)
//          case _ => (ul -> null)
//        }
//      }.filter{case (ul, v) => v != null}
//    }
  }


  def getHistorical(id:String):TimeSeries = getHistorical(id, null)
	  
  def getHistorical(id:String, paramType:String):TimeSeries = getHistorical(id, null, null, paramType)
  
  def getHistorical(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries = repository match {
    case Some(repo) => repo.getHistorical(id, startDate, endDate, assetId)
    case None => TimeSeries.empty
  }

  def getHistoricalLow(id:String):TimeSeries = getHistoricalLow(id, null)
    
  def getHistoricalLow(id:String, paramType:String):TimeSeries = getHistoricalLow(id, null, null, paramType)
  
  def getHistoricalLow(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries = repository match {
    case Some(repo) => repo.getHistoricalLow(id, startDate, endDate, assetId)
    case None => TimeSeries.empty
  }
  
  def getHistoricalHigh(id:String):TimeSeries = getHistoricalHigh(id, null)
    
  def getHistoricalHigh(id:String, paramType:String):TimeSeries = getHistoricalHigh(id, null, null, paramType)
  
  def getHistoricalHigh(id:String, startDate:Date, endDate:Date, assetId:String = null):TimeSeries = repository match {
    case Some(repo) => repo.getHistoricalHigh(id, startDate, endDate, assetId)
    case None => TimeSeries.empty
  }
  
  def getForwardPrices(assetId:String, id:String):TimeSeries = repository.collect{case repo => repo.getForwardPrices(assetId, id)}.getOrElse(TimeSeries.empty)
  
  def getEquities:Set[Equity] = repository.collect{case repo => repo.getEquities}.getOrElse(Set.empty)
  
  def getIndices:Set[Index] = repository.collect{case repo => repo.getIndices}.getOrElse(Set.empty)
  
  def getHolidays(id:String):Set[Date] = repository.collect{case repo => repo.getHolidays(id)}.getOrElse(Set.empty)

  def getCountryIds:Set[String] = repository.collect{case repo => repo.getCountryIds}.getOrElse(Set.empty)

  def getCurrencyIds:Set[String] = repository.collect{case repo => repo.getCurrencyIds}.getOrElse(Set.empty)

}
