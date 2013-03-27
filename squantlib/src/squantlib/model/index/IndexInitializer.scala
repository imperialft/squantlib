package squantlib.model.index

import squantlib.model.Market
import squantlib.model.yieldparameter.YieldParameter
import squantlib.database.schemadefinitions.RateFXParameter
import org.jquantlib.time.{Period => qlPeriod}
import org.jquantlib.time.{Period => qlPeriod}

/**
 * Index specific discount curve calibration.
 */
trait IndexInitializer {
  
  def getModel(market:Market):Option[Index]
  
}

object IndexInitializer {
  
  def getInitializers(params:Set[RateFXParameter]):Map[String, IndexInitializer] = {
    val paramsets:Map[String, Set[RateFXParameter]] = params.filter(p => (modelMap contains p.asset)).groupBy(_.asset)
    paramsets.map{case (name, ps) => (name, modelMap(name)(ps))}
  }
  
  val modelMap:Map[String, Set[RateFXParameter] => IndexInitializer] = Map(
      "NKY" -> (p => IndexATMContinuous("NKY", p, "JPY", "JPY", 0.00))
  )
  
}

case class IndexATMContinuous(
    name:String, 
    indexparams:Set[RateFXParameter], 
    ccy:String,
    discountCcy:String = "USD",
    discountSpread:Double = 0.00
    ) extends IndexInitializer {
  
  val yieldid = "Yield"
  val spotid = "Index"
  val volid = "IndexVol"
  val repoid = "Repo"
  
  override def getModel(market:Market):Option[Index] = {
    val params = indexparams.groupBy(_.instrument)
    if (!params.contains(yieldid) || !params.contains(spotid)) {return None}
    
    val valuedate = market.valuedate
    val yldparam:Map[qlPeriod, Double] = params(yieldid).map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
    val dividend = DividendCurve(valuedate, yldparam).orNull
    if (dividend == null) {return None}
    
    val spot:Double = params(spotid).head.value
    
    val ratecurve = market.getDiscountCurve(ccy, discountCcy, discountSpread).orNull
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
     
    Some(SmoothIndex(name, spot, ratecurve, dividend, repo, vol))
  }

}