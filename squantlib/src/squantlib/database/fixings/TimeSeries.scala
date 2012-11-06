package squantlib.database.fixings

import java.util.{Date => JavaDate}
import squantlib.setting.initializer.Currencies
import squantlib.database.DB

/**
* Functions to access from database by index name
*/

object TimeSeries {
  
	def apply(id:String):Map[JavaDate, Double] = mapper.getOrElse(id, Map.empty)
	
	def getOrElse(id:String, defaultvalue:Map[JavaDate, Double]):Map[JavaDate, Double] = mapper.getOrElse(id, defaultvalue)
	
	def contains(s:String):Boolean = mapper.contains(s)
	
	def keySet:Set[String] = mapper.keySet
  
  	val cmtMap:Map[String, Map[JavaDate, Double]] = Map("CMT10" -> DB.getRateFXTimeSeries("Fixing", "JGBY", "10Y"))
  	
  	val currencySet = Currencies.keySet
  
	val cashMaturities = Set("1M", "2M", "3M", "6M")
	
	val cashMap = for (c <- currencySet; m <- cashMaturities) yield ((c + m) -> DB.getRateFXTimeSeries("Cash", c, m))
	
    val swapMaturities = Set("1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y")
    
    val swapMap = for (c <- currencySet; m <- swapMaturities) yield ((c + m) -> DB.getRateFXTimeSeries("Swap", c, m))
    
    val fxMap = for (c1 <- currencySet; c2 <- currencySet) yield ((c1 + c2) -> DB.getFXTimeSeries(c1, c2))
	    
    val mapper:Map[String, Map[JavaDate, Double]] = cmtMap ++ fxMap ++ cashMap ++ swapMap
}


