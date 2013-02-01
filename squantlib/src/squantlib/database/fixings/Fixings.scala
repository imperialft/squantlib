package squantlib.database.fixings

import squantlib.database.DB
import squantlib.setting.initializer.Currencies
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}


object Fixings {
  
	val fixingcache = new scala.collection.mutable.WeakHashMap[String, Option[Map[qlDate, Double]]]
	
	val latestParamDate = new qlDate(DB.getLatestParamSet._2)
  
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
	  
	def latest(id:String):Option[(qlDate, Double)] = apply(id).collect{case p => p.maxBy(_._1)}
	
	def getHistorical(param:String, paramType:String = null):Option[Map[qlDate, Double]] = 
	  if (param == null) None
	  else (param.trim match {
	    case "CMT10" => Some(DB.getRateFXParams("Fixing", "JGBY", "10Y"))
	    case "NKY" => Some(DB.getRateFXParams("Fixing", "NKY"))
	    case p if p.head.isDigit => Some(DB.getRateFXParams("Equity", p))
	    case p if paramType != null => Some(DB.getRateFXParams(paramType, p))
	    case p if p.size <= 3 => None
	    case p => (p take 3, p substring 3) match {
	      case (ccy, _) if !isCcy(ccy) => None
	      case (ccy1, "JPY") => Some(DB.getFXParams(ccy1))
	      case ("JPY", ccy2) if isCcy(ccy2) => Some(DB.getFXParams(ccy2).collect{case (d, n) => (d, 1.0 / n)})
	      case (ccy1, ccy2) if isCcy(ccy2) => Some(DB.getFXParams(ccy1, ccy2))
	      case (ccy, mat) if !isNumber(mat dropRight 1) => None
	      case (ccy, mat) if cashPeriods contains (mat takeRight 1) => Some(DB.getRateFXParams("Cash", ccy, mat))
	      case (ccy, mat) if swapPeriods contains (mat takeRight 1) => Some(DB.getRateFXParams("Swap", ccy, mat))
	      case _ => None
	  }
	}).collect{case m:Map[JavaDate, Double] => m.map{case (d, v) => (new qlDate(d), v)}}
 	  
  	val currencySet = Currencies.keySet
	private def isCcy(v:String):Boolean = currencySet contains v
	private val cashPeriods = Set("M", "W", "D")
	private val swapPeriods = Set("Y")
	private def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _ => false}
	
}


