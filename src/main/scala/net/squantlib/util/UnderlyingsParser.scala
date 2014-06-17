package net.squantlib.util

import net.squantlib.model.market.Market
import net.squantlib.model.asset.Underlying
import net.squantlib.util.initializer.Currencies
import net.squantlib.math.timeseries.TimeSeries
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

  def quotedCurrency:Option[String]
  
}


