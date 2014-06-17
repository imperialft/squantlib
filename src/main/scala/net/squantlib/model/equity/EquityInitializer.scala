package net.squantlib.model.equity

import net.squantlib.model.market.Market
import net.squantlib.model.yieldparameter.YieldParameter
import net.squantlib.database.DB
import net.squantlib.database.schemadefinitions.{Equity => EquityInfo}
import net.squantlib.util.Date
import net.squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Period => qlPeriod}
import scala.collection.mutable.{Map => MutableMap}
import scala.annotation.tailrec

/**
 * Equity specific discount curve calibration.
 */
trait EquityInitializer {
  
  def getModel(market:Market):Option[Equity]
  
  def mult(x:Double):EquityInitializer
  
  def addVol(x:Double):EquityInitializer
  
  def addDividend(x:Double):EquityInitializer
}


case class EmptyInitializer() extends EquityInitializer {
  override def getModel(market:Market):Option[Equity] = None
  override def mult(x:Double):EquityInitializer = this
  override def addVol(x:Double):EquityInitializer = this
  override def addDividend(x:Double):EquityInitializer = this
}

case class FlatDivATM(
    name:String, 
    ccy:String,
    spot:Double, 
    dividends:Map[Date, Double],
    repo:Map[qlPeriod, Double],
    vol:Map[qlPeriod, Double],
    discountCurve:String,
    discountSpread:Double = 0.0
    ) extends EquityInitializer {
  
  
  override def getModel(market:Market):Option[Equity] = {
    val valuedate = market.valuedate
    
    val ratecurve:DiscountCurve = market.getDiscountCurve(ccy, discountCurve, discountSpread).orNull
    if (ratecurve == null) {return None}
    
    val repoCurve:RepoCurve = 
      if (repo.isEmpty) RepoCurve.zeroCurve(valuedate) 
      else RepoCurve(valuedate, repo).getOrElse(RepoCurve.zeroCurve(valuedate))
    
    val volCurve:YieldParameter = 
      if (vol.isEmpty) YieldParameter(valuedate, Double.NaN).get
      else YieldParameter(valuedate, vol).getOrElse(YieldParameter(valuedate, Double.NaN).get)
     
    Some(BasicEquity(name, spot, ratecurve, dividends, repoCurve, volCurve))
  }
  
  override def mult(x:Double):EquityInitializer = FlatDivATM(
    name, 
    ccy,
    spot * x, 
    dividends,
    repo,
    vol,
    discountCurve,
    discountSpread)
    
  override def addVol(x:Double):EquityInitializer = FlatDivATM(
    name, 
    ccy,
    spot, 
    dividends,
    repo,
    vol.map{case (k, v) => (k, v + x)},
    discountCurve,
    discountSpread)

  override def addDividend(x:Double):EquityInitializer = FlatDivATM(
    name, 
    ccy,
    spot, 
    dividends.map{case (k, v) => (k, v + x)},
    repo,
    vol,
    discountCurve,
    discountSpread)
}






