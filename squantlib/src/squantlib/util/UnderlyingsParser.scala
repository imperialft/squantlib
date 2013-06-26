package squantlib.util

import squantlib.model.{Underlying, Market}
import squantlib.database.fixings.Fixings
import squantlib.util.initializer.Currencies
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}
import java.util.{Date => JavaDate}
import squantlib.database.DB
import squantlib.util

object UnderlyingParser {
  
  val parsers:List[UnderlyingParser] = List(
      new CmtParser,
      new IndexParser,
      new JGBPriceParser,
      new EquityParser,
      new FxParser,
      new CashParser,
      new SwapParser
  )
  
  def getParser(id:String):Option[UnderlyingParser] = parsers.filter(p => p.isApplicable(id)).headOption
  
  def getUnderlying(id:String, mkt:Market):Option[Underlying] = getParser(id).flatMap{case s => s.getUnderlying(id, mkt)}
  
  def getHistorical(id:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = getParser(id).collect{case s => s.getHistorical(id, start, end)}.getOrElse(Map.empty)

  def getHistorical(assetType:String, id:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = (new CustomParameterParser).getHistorical(assetType, id, start, end)
}


abstract class UnderlyingParser{
  def isApplicable(id:String):Boolean
  def getUnderlying(id:String, mkt:Market):Option[Underlying] 
  def getHistorical(id:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double]

  def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _:Throwable => false}
  def rateBreakDown(id:String) = (id take 3, (id substring 3) dropRight 1, id takeRight 1)
  def isCurrency(id:String) = Currencies.keySet contains id
}

case class CmtParser extends UnderlyingParser {
  override def isApplicable(id:String) = id == "CMT10"
  override def getUnderlying(id:String, mkt:Market) = None
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = DB.getHistoricalRateFX("Fixing", "JGBY", "10Y", start, end)
}

case class IndexParser extends UnderlyingParser {
  override def isApplicable(id:String) = id == "NKY"
  override def getUnderlying(id:String, mkt:Market) = mkt.getIndex(id)
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = DB.getHistoricalRateFX("Index", id, start, end)
}

case class JGBPriceParser extends UnderlyingParser {
  override def isApplicable(id:String) = (id take 4) == "JGBM"
  override def getUnderlying(id:String, mkt:Market) = None
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = DB.getHistoricalJsdaPrice(id, start, end)
}

case class EquityParser extends UnderlyingParser {
  override def isApplicable(id:String) = id.head isDigit
  override def getUnderlying(id:String, mkt:Market) = mkt.getEquity(id)
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = DB.getHistoricalRateFX("Equity", id, start, end)
}

case class FxParser extends UnderlyingParser {
  override def isApplicable(id:String) = id.size == 6 && isCurrency(id take 3) && isCurrency(id takeRight 3)
  override def getUnderlying(id:String, mkt:Market) = mkt.getFX(id)
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = (id take 3, id takeRight 3) match {
    case (ccy1, "JPY") => DB.getFXParams(ccy1, start, end)
    case ("JPY", ccy2) => DB.getFXParams(ccy2, start, end).collect{case (d, n) => (d, 1.0 / n)}
    case (ccy1, ccy2) => DB.getFXParams(ccy1, ccy2, start, end)
  }
}

case class CashParser extends UnderlyingParser {
  val cashPeriods = Set("M", "W", "D")
  override def isApplicable(id:String) = id.size > 3 && (rateBreakDown(id) match {case (ccy, mat, per) => isCurrency(ccy) && isNumber(mat) && (cashPeriods contains per)})
  override def getUnderlying(id:String, mkt:Market) = rateBreakDown(id) match {
    case (ccy, mat, per) => mkt.getCashUnderlying(ccy, new qlPeriod(mat + per))
  }
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = rateBreakDown(id) match {
    case (ccy, mat, per) => DB.getHistoricalRateFX("Cash", ccy, mat + per, start, end)
  }
}

case class SwapParser extends UnderlyingParser {
  val swapPeriods = Set("Y")
  override def isApplicable(id:String) = id.size > 3 && (rateBreakDown(id) match {case (ccy, mat, per) => isCurrency(ccy) && isNumber(mat) && (swapPeriods contains per)})
  override def getUnderlying(id:String, mkt:Market) = None
  override def getHistorical(id:String, start:JavaDate, end:JavaDate) = rateBreakDown(id) match {
    case (ccy, mat, per) => DB.getHistoricalRateFX("Swap", ccy, mat + per, start, end)
  }
}

case class CustomParameterParser {
  def getUnderlying(assetId:String, id:String, mkt:Market) = None
  def getHistorical(assetId:String, id:String, start:JavaDate, end:JavaDate) = DB.getHistoricalRateFX(assetId, id, start, end)
}

