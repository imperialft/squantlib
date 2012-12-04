package squantlib.database.fixings

import squantlib.database.DB
import squantlib.setting.initializer.Currencies
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}


object Fixings {
  
	def apply(id:String):Option[(JavaDate, Double)] = apply(id, new JavaDate)
	  
	def apply(id:String, vd:JavaDate):Option[(JavaDate, Double)] = {
		if (mapper.contains(id)) mapper(id)(vd)
		else None
	}
	
	def apply(id:String, vd:qlDate):Option[(JavaDate, Double)] = apply(id, vd.longDate)
	
	def getOrElse(id:String, vd:JavaDate, defaultvalue:(JavaDate, Double)):(JavaDate, Double) = apply(id, vd).getOrElse(defaultvalue)
	def contains(s:String):Boolean = mapper.contains(s)
	def keySet:Set[String] = mapper.keySet
  
  	val cmtMap:Map[String, JavaDate => Option[(JavaDate, Double)]] = 
  	  Map("CMT10" -> ((d:JavaDate) => DB.getLatestRateFXParamSet("Fixing", "JGBY", "10Y", d)))
  	
  	val currencySet = Currencies.keySet
  
	val cashMaturities = Set("1M", "2M", "3M", "6M")
	val cashMap = for (c <- currencySet; m <- cashMaturities) yield ((c + m) -> ((d:JavaDate) => DB.getLatestRateFXParamSet("Cash", c, m, d)))
	
    val swapMaturities = Set("1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y")
    val swapMap = for (c <- currencySet; m <- swapMaturities) yield ((c + m) -> ((d:JavaDate) => DB.getLatestRateFXParamSet("Swap", c, m, d)))
    
    val fxMap = 
      currencySet.map(c => (c + "JPY") -> ((d:JavaDate) => DB.getLatestFXParamSet(c, d))) ++ 
      currencySet.map(c => ("JPY" + c) -> ((d:JavaDate) => DB.getLatestFXParamSet(c, d).collect{case (d, n) => (d, 1/n)})) ++
      (for (c1 <- (currencySet - "JPY"); c2 <- (currencySet - "JPY")) yield((c1 + c2) -> ((d:JavaDate) => DB.getLatestFXParamSet(c1, c2, d))))
	    
    val mapper:Map[String, JavaDate => Option[(JavaDate, Double)]] = cmtMap ++ fxMap ++ cashMap ++ swapMap
}


