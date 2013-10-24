package squantlib.util

import squantlib.model.Market
import squantlib.model.asset.Underlying
import squantlib.util.initializer.Currencies
import squantlib.math.timeseries.TimeSeries
import java.util.{Date => JavaDate}

object UnderlyingParsers {
  
  private var repository:List[UnderlyingParserGenerator[_]] = List.empty // to be initialized
  
  def setParsers(p:List[UnderlyingParserGenerator[_]]) = repository = p
  
  def getRepository = repository
  
  def get(id:String):Option[UnderlyingParser] = 
    if (id == null) None 
    else repository.map(r => r.getParser(id) match {
        case Some(p:UnderlyingParser) => Some(p)
        case _ => None
        }).collect{case Some(p) => p}.headOption
  
  def getUnderlying(id:String, mkt:Market):Option[Underlying] = 
    if (id == null) None else get(id).flatMap{case s => s.getUnderlying(mkt)}
  
  def isCurrency(id:String):Boolean = Currencies.keySet contains id
  
  def extractCurrencies(id:String):Set[String] = 
    if (id == null) Set.empty else id.grouped(3).filter(isCurrency).toSet
  
//  
//  def getSpot(id:String, mkt:Market):Option[Double] = get(id).flatMap{case s => s.getSpot(id, mkt)}
//  
//  def getHistorical(id:String, start:Date, end:Date):TimeSeries = 
//    get(id).collect{case s => s.getHistorical(id, start, end)}.getOrElse(TimeSeries.empty)
//	
//  val typeCmt = "CMT"
//    
//  val typeIndex = "INDEX"
//    
//  val typeJGBPrice = "JGB"
//  
//  val typeEquity = "EQUITY"
//    
//  val typeFX = "FX"
//    
//  val typeCash = "CASH"
//    
//  val typeSwap = "SWAP"
//    
//  val typeCcy = "CURRENCY"
//  
//  val typeBondPrice = "PRICE"
//    
//  val typeJpyBondPrice = "JPYPRICE"
  
}

trait UnderlyingParserGenerator[T <: UnderlyingParser] {
  
  def getParser(id:String):Option[T]
  
}

trait UnderlyingParser {

  val id:String
  
  def getUnderlying(mkt:Market):Option[Underlying] 
  
  def getHistorical(start:Date, end:Date):TimeSeries
  
  def getSpot(mkt:Market):Option[Double] = getUnderlying(mkt).collect{case u => u.spot}
  
  def getShiftedMarket(shift:Double, mkt:Market):Option[Market]
	
  def getShiftedVolMarket(shift:Double, mkt:Market):Option[Market] 

  
}


