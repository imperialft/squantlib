package squantlib.model.equity

import squantlib.model.Market
import squantlib.model.yieldparameter.YieldParameter
import squantlib.database.schemadefinitions.{RateFXParameter, Equity => EquityInfo}
import squantlib.database.DB
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}
import org.jquantlib.time.BusinessDayConvention
import org.jquantlib.time.calendars.NullCalendar
import scala.collection.mutable.{Map => MutableMap}
import org.jquantlib.time.Calendar
import scala.annotation.tailrec
import squantlib.util.initializer.Calendars

/**
 * Equity specific discount curve calibration.
 */
trait EquityInitializer {
  
  def getModel(market:Market):Option[Equity]
  
  def mult(x:Double):EquityInitializer
  
  def addVol(x:Double):EquityInitializer
  
  def addDividend(x:Double):EquityInitializer
}

object EquityInitializer {
  
  val equitylist:Map[String, EquityInfo] = DB.getEquities.map(e => (e.id, e)) (collection.breakOut)
    
  def getInitializers(params:Set[RateFXParameter]):Map[String, EquityInitializer] = {
    val paramsets:Map[String, Set[RateFXParameter]] = params.filter(p => (equitylist contains p.asset)).groupBy(_.asset)
    paramsets.map{case (name, ps) => (name, modelMap(name)(ps))}
  }
  
  val modelMap:Map[String, Set[RateFXParameter] => EquityInitializer] = 
    equitylist.map{case (name, eqty) => (name, (p:Set[RateFXParameter]) => SimpleInitializer(name, p, eqty, eqty.currencyid, 0.0))
  }
}

case class EmptyInitializer extends EquityInitializer {
  override def getModel(market:Market):Option[Equity] = None
  override def mult(x:Double):EquityInitializer = this
  override def addVol(x:Double):EquityInitializer = this
  override def addDividend(x:Double):EquityInitializer = this
}

case class FlatDivATM(
    name:String, 
    ccy:String,
    spot:Double, 
    dividends:Map[qlDate, Double],
    repo:Map[qlPeriod, Double],
    vol:Map[qlPeriod, Double],
    discountCurve:String,
    discountSpread:Double = 0.0
    ) extends EquityInitializer {
  
  
  override def getModel(market:Market):Option[Equity] = {
    val valuedate = market.valuedate
    
    val ratecurve:squantlib.model.rates.DiscountCurve = market.getDiscountCurve(ccy, discountCurve, discountSpread).orNull
    if (ratecurve == null) {return None}
    
    val repoCurve:squantlib.model.equity.RepoCurve = 
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


object SimpleInitializer {
  val dividendid = "Dividend12M"
  val spotid = "Equity"
  val volid = "EquityVol"
  val repoid = "Repo"
  
  def apply(
    name:String, 
    equityparams:Set[RateFXParameter], 
    equityinfo:EquityInfo,
    discountCurve:String,
    discountSpread:Double = 0.0):EquityInitializer = {
    
    val params = equityparams.groupBy(_.instrument)
    if (!params.contains(dividendid) || !params.contains(spotid)) {return new EmptyInitializer}
    
    val baseDivDate = new qlDate(equityinfo.basedivdate)
    val lastDivDate = baseDivDate.add(new qlPeriod("30Y"))
    
    val ccy = equityinfo.currencyid

    val spot:Double = params(spotid).head.value
    
    val annualdiv:Double = params(dividendid).head.value
    val divfreq = equityinfo.divfreq
    val dividends:Map[qlDate, Double] = constractDividend(baseDivDate, lastDivDate, annualdiv, divfreq, Set(ccy))
    if (dividends == null || dividends.isEmpty) {return new EmptyInitializer}
    
    val repo:Map[qlPeriod, Double] = (params.get(repoid) match {
      case Some(rs) => rs.map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
      case None => Map.empty
    })
    
    val vol:Map[qlPeriod, Double] = (params.get(volid) match {
      case Some(vols) => vols.map(p => (new qlPeriod(p.maturity), p.value)).toMap
      case None => if (equityinfo.volatility > 0) Map(new qlPeriod("3M") -> equityinfo.volatility) else Map.empty
    })
     
    FlatDivATM(name, ccy, spot, dividends, repo, vol, discountCurve, discountSpread)
  }
  
  def constractDividend(baseDate:qlDate, endDate:qlDate, annualAmount:Double, payFreq:Int, calendars:Set[String]):Map[qlDate, Double] = {
    val cdr = Calendars(calendars).getOrElse(new NullCalendar)
    val tenor = new qlPeriod(payFreq + "M")
    var currentdate = baseDate
    var periods = 1
    var divamount = annualAmount / 12.0 * payFreq.toDouble
    
    @tailrec def scheduleIter(currentPeriod:Int, currentmap:Map[qlDate, Double]):Map[qlDate, Double] = {
      val nextDate = cdr.advance(baseDate, tenor.mul(currentPeriod), BusinessDayConvention.Following)
      if (nextDate ge endDate) currentmap ++ Map((nextDate, divamount), (baseDate, divamount))
      else scheduleIter(currentPeriod + 1, currentmap + (nextDate -> divamount))
    }
    
    scheduleIter(1, Map.empty)
    
  }

}



