package squantlib.database.fixings

import squantlib.database.DB
import squantlib.util.initializer.Currencies
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}


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
 	
	def getHistorical(param:String, startDate:qlDate, endDate:qlDate, paramType:String = null):Option[Map[qlDate, Double]] = {
	  val start = if (startDate == null) null else startDate.longDate
	  val end = if (endDate == null) null else endDate.longDate
	  
	  if (param == null) None
	  else (param.trim match {
	    case "CMT10" => Some(DB.getHistoricalRateFX("Fixing", "JGBY", "10Y", start, end))
	    case "NKY" => Some(DB.getHistoricalRateFX("Index", "NKY", start, end))
	    case p if p.take(4) == "JGBM" => Some(DB.getHistoricalJsdaPrice(p, start, end))
	    case p if p.head.isDigit => Some(DB.getHistoricalRateFX("Equity", p, start, end))
	    case p if paramType != null => Some(DB.getHistoricalRateFX(paramType, p, start, end))
	    case p if p.size <= 3 => None
	    case p => (p take 3, p substring 3) match {
	      case (ccy, _) if !isCcy(ccy) => None
	      case (ccy1, "JPY") => Some(DB.getFXParams(ccy1, start, end))
	      case ("JPY", ccy2) if isCcy(ccy2) => Some(DB.getFXParams(ccy2, start, end).collect{case (d, n) => (d, 1.0 / n)})
	      case (ccy1, ccy2) if isCcy(ccy2) => Some(DB.getFXParams(ccy1, ccy2, start, end))
	      case (ccy, mat) if !isNumber(mat dropRight 1) => None
	      case (ccy, mat) if cashPeriods contains (mat takeRight 1) => Some(DB.getHistoricalRateFX("Cash", ccy, mat, start, end))
	      case (ccy, mat) if swapPeriods contains (mat takeRight 1) => Some(DB.getHistoricalRateFX("Swap", ccy, mat, start, end))
	      case _ => None
	  }
	  }).collect{case m:Map[JavaDate, Double] => m.map{case (d, v) => (new qlDate(d), v)}}
	}
 	  
  	val currencySet = Currencies.keySet
  	
	def isCcy(v:String):Boolean = currencySet contains v
	
	private val cashPeriods = Set("M", "W", "D")
	
	private val swapPeriods = Set("Y")
	
	private def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _:Throwable => false}
	
}


