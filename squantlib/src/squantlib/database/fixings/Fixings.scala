package squantlib.database.fixings

import squantlib.database.DB
import squantlib.util.initializer.Currencies
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import squantlib.util.UnderlyingParser

object Fixings {
  
	val fixingcache = new scala.collection.mutable.WeakHashMap[String, Option[Map[qlDate, Double]]]
	
	lazy val latestParamDate = new qlDate(DB.getLatestParamSet._2)
  
	def apply(id:String):Option[Map[qlDate, Double]] = fixingcache.getOrElseUpdate(id, getHistorical(id))
	
	def byDate(id:String, vd:JavaDate):Option[(qlDate, Double)] = byDate(id, new qlDate(vd))
	
	def byDate(id:String, vd:qlDate):Option[(qlDate, Double)] = 
	  if (vd gt latestParamDate) None 
	  else apply(id).flatMap{
	    case m => m.filterKeys(d => d le vd) match {
	      case mp if mp.isEmpty => None
	      case mp => Some(mp.maxBy(_._1))
	    }
	  }
		  
	def latest(id:String):Option[(qlDate, Double)] = apply(id).collect{case p if !p.isEmpty => p.maxBy(_._1)}
	
	def latestList(ids:List[String]):Map[String, Double] = ids.map(id => (id, Fixings.latest(id))).collect{case (a, Some(b)) => (a, b._2)} (collection.breakOut)
	
	def pastFixings(params:Set[String], dates:List[qlDate], paramType:String = null):List[Map[String, Option[Double]]] = {
	  if (dates.isEmpty) {return List.empty}
	  val allhistory:Map[String, Option[Map[qlDate, Double]]] = params.map(p => (p, getHistorical(p, dates.min, dates.max, paramType))) (collection.breakOut)
	  dates.map(d => params.map(p => 
	    (p, allhistory(p).flatMap{case ps => ps.get(d)})).toMap)
	}
	
	def getHistorical(param:String):Option[Map[qlDate, Double]] = getHistorical(param, null)
	  
	def getHistorical(param:String, paramType:String):Option[Map[qlDate, Double]] = getHistorical(param, null, null, paramType)
 	  
	def getHistorical(param:String, startDate:qlDate, endDate:qlDate, assetId:String = null):Option[Map[qlDate, Double]] = {
	  val start:JavaDate = if (startDate == null) null else startDate.longDate
	  val end:JavaDate = if (endDate == null) null else endDate.longDate
	  
	  (if (assetId == null) UnderlyingParser.getHistorical(param, start, end)
	  else UnderlyingParser.getHistorical(assetId, param, start, end)) match {
	      case r if r.isEmpty => None
	      case r => Some(r.map{case (d, v) => (new qlDate(d), v)})
	  }
	}
	
  	val currencySet = Currencies.keySet
  	
	def isCcy(v:String):Boolean = currencySet contains v
	
	private val cashPeriods = Set("M", "W", "D")
	
	private val swapPeriods = Set("Y")
	
	private def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _:Throwable => false}
	
}


