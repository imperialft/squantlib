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

/**
 * Equity specific discount curve calibration.
 */
trait EquityInitializer {
  
  def getModel(market:Market):Option[Equity]
  
}

object EquityInitializer {
  
  val equitylist:Map[String, EquityInfo] = DB.getEquities.map(e => (e.id, e)) (collection.breakOut)
    
  def getInitializers(params:Set[RateFXParameter]):Map[String, EquityInitializer] = {
    val paramsets:Map[String, Set[RateFXParameter]] = params.filter(p => (equitylist contains p.asset)).groupBy(_.asset)
    paramsets.map{case (name, ps) => (name, modelMap(name)(ps))}
  }
  
  val modelMap:Map[String, Set[RateFXParameter] => EquityInitializer] = 
    equitylist.map{case (name, eqty) => (name, (p:Set[RateFXParameter]) => EquityFlatDivATM(name, p, eqty, eqty.currencyid, 0.0))
  }
}

case class EquityFlatDivATM(
    name:String, 
    equityparams:Set[RateFXParameter], 
    equityinfo:EquityInfo,
    discountCurve:String,
    discountSpread:Double = 0.0
    ) extends EquityInitializer {
  
  val dividendid = "Dividend12M"
  val spotid = "Equity"
  val volid = "EquityVol"
  val repoid = "Repo"
  
  val baseDivDate = new qlDate(equityinfo.basedivdate)
  val lastDivDate = baseDivDate.add(new qlPeriod("30Y"))
  
  override def getModel(market:Market):Option[Equity] = {
    val params = equityparams.groupBy(_.instrument)
    if (!params.contains(dividendid) || !params.contains(spotid)) {return None}

    val valuedate = market.valuedate
    val spot:Double = params(spotid).head.value
    
    val annualdiv = params(dividendid).head.value
    val divfreq = equityinfo.divfreq
    val ccy = equityinfo.currencyid
    
    val dividends:Map[qlDate, Double] = constractDividend(baseDivDate, lastDivDate, annualdiv, divfreq, Set(ccy))
    if (dividends == null || dividends.isEmpty) {return None}
    
    val ratecurve = market.getDiscountCurve(ccy, discountCurve, discountSpread).orNull
    if (ratecurve == null) {return None}
    
    val repo = (params.get(repoid) match {
      case Some(rs) => 
        val repoparam:Map[qlPeriod, Double] = rs.map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
        RepoCurve(valuedate, repoparam)
      case None => None
    }).getOrElse(RepoCurve.zeroCurve(valuedate))
    
    val vol:YieldParameter = (params.get(volid) match {
      case Some(vols) => YieldParameter(valuedate, vols.map(p => (new qlPeriod(p.maturity), p.value)).toMap)
      case None => None
    }).getOrElse(YieldParameter(valuedate, Double.NaN).get)
     
    Some(BasicEquity(name, spot, ratecurve, dividends, repo, vol))
  }
  
  def constractDividend(baseDate:qlDate, endDate:qlDate, annualAmount:Double, payFreq:Int, calendars:Set[String]):Map[qlDate, Double] = {
    val cdr = squantlib.setting.initializer.Calendars(calendars).getOrElse(new NullCalendar)
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