package squantlib.util

import squantlib.model.Market
import squantlib.model.asset.Underlying
import squantlib.util.initializer.Currencies
import java.util.{Date => JavaDate}
import squantlib.util

object UnderlyingParser {
  
  private var parsers:List[UnderlyingParser] = List.empty
  
  def setParsers(p:List[UnderlyingParser]) = parsers = p
  def getParsers = parsers
  
  def getParser(id:String):Option[UnderlyingParser] = parsers.filter(p => p.isApplicable(id)).headOption
  
  def getUnderlying(id:String, mkt:Market):Option[Underlying] = getParser(id).flatMap{case s => s.getUnderlying(id, mkt)}
  
  def getSpot(id:String, mkt:Market):Option[Double] = getParser(id).flatMap{case s => s.getSpot(id, mkt)}
  
  def getHistorical(id:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = getParser(id).collect{
    case s => s.getHistorical(id, start, end)}.getOrElse(Map.empty)

  val typeCmt = "CMT"
    
  val typeIndex = "INDEX"
    
  val typeJGBPrice = "JGB"
  
  val typeEquity = "EQUITY"
    
  val typeFX = "FX"
    
  val typeCash = "CASH"
    
  val typeSwap = "SWAP"
    
  val typeCcy = "CURRENCY"
  
  val typeBondPrice = "PRICE"
    
  val typeJpyBondPrice = "JPYPRICE"
  
}


abstract class UnderlyingParser{
  def isApplicable(id:String):Boolean
  def getUnderlying(id:String, mkt:Market):Option[Underlying] 
  def getHistorical(id:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double]
  def getSpot(id:String, mkt:Market):Option[Double] = getUnderlying(id, mkt).collect{case u => u.spot}

  def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _:Throwable => false}
  def rateBreakDown(id:String) = (id take 3, (id substring 3) dropRight 1, id takeRight 1)
  def isCurrency(id:String) = Currencies.keySet contains id
  
  def getType:String
}

