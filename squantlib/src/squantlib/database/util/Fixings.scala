package squantlib.database.util

import squantlib.database.DB
import squantlib.initializer.Currencies
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}

object Fixings {
  
  def apply(name:String, paramset:String):Option[Double] = fixings.get(name) match {
    	case Some(f) => f(paramset); case None => None}
  
  def apply(name:String, date:JavaDate):Option[Double] = apply(name, ("%tY%<tm%<td" format date) + "-000")
  def apply(name:String, date:qlDate):Option[Double] = apply(name, date.longDate)
  
  val cmt = Map("CMT10" -> ((p:String) => DB.getRateFX(p, "Fixing", "JGBY", "10Y")))
  
  val cash = {
    val maturities = Set("3M", "6M")
    for (c <- Currencies.keySet; m <- maturities) 
      yield ((c + m) -> ((p:String) => DB.getRateFX(p, "Cash", c, m)))
  }
  
  val swap = {
    val maturities = Set("1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y")
    for (c <- Currencies.keySet; m <- maturities) 
      yield ((c + m) -> ((p:String) => DB.getRateFX(p, "Swap", c, m)))
  }
    
  val fxjpy = Currencies.keySet.map(c => (c + "JPY") -> ((p:String) => DB.getFX(p, c))) ++
	Currencies.keySet.map(c => ("JPY" + c) -> ((p:String) => DB.getFX(p, c).collect{case n => 1/n}))
	   
  val fxother = for (c1 <- (Currencies.keySet - "JPY"); c2 <- (Currencies.keySet - "JPY"))
    yield((c1 + c2) -> ((p:String) => DB.getFX(p, c1, c2)))
	    
  val fixings = cmt ++ fxjpy ++ fxother ++ cash ++ swap
}


