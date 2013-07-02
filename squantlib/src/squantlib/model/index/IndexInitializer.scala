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
  
  def mult(x:Double):IndexInitializer
  
  def addVol(x:Double):IndexInitializer
  
  def addDividend(x:Double):IndexInitializer
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

case class EmptyInitializer extends IndexInitializer {
  override def getModel(market:Market):Option[Index] = None
  override def mult(x:Double):IndexInitializer = this
  override def addVol(x:Double):IndexInitializer = this
  override def addDividend(x:Double):IndexInitializer = this
}


case class IndexATMContinuous(
    name:String, 
    ccy:String,
    spot:Double,
    divYield:Map[qlPeriod, Double],
    repo:Map[qlPeriod, Double],
    vol:Map[qlPeriod, Double],
    discountCcy:String,
    discountSpread:Double
    ) extends IndexInitializer {
  
  override def getModel(market:Market):Option[Index] = {
    
    val valuedate = market.valuedate
    
    val dividend:DividendCurve = DividendCurve(valuedate, divYield).orNull
    if (dividend == null) {return None}
    
    val ratecurve = market.getDiscountCurve(ccy, discountCcy, discountSpread).orNull
    if (ratecurve == null) {return None}
    
    val repoCurve:RepoCurve = if (repo.isEmpty) RepoCurve.zeroCurve(valuedate) else RepoCurve(valuedate, repo).getOrElse(RepoCurve.zeroCurve(valuedate))
    
    val volCurve:YieldParameter = if (vol.isEmpty) YieldParameter(valuedate, Double.NaN).get else YieldParameter(valuedate, vol).getOrElse(YieldParameter(valuedate, Double.NaN).get)
    
    Some(SmoothIndex(name, spot, ratecurve, dividend, repoCurve, volCurve))
  }
  
  override def mult(x:Double):IndexInitializer = IndexATMContinuous(
    name, 
    ccy,
    spot * x,
    divYield,
    repo,
    vol,
    discountCcy,
    discountSpread
  )
  
  override def addVol(x:Double):IndexInitializer = IndexATMContinuous(
    name, 
    ccy,
    spot,
    divYield,
    repo,
    vol.map{case (t, v) => (t, v+x)},
    discountCcy,
    discountSpread
  )

  override def addDividend(x:Double):IndexInitializer = IndexATMContinuous(
    name, 
    ccy,
    spot,
    divYield.map{case (t, v) => (t, v+x)},
    repo,
    vol,
    discountCcy,
    discountSpread
  )
}

object IndexATMContinuous {
  
  val yieldid = "Yield"
  val spotid = "Index"
  val volid = "IndexVol"
  val repoid = "Repo"
  
  def apply(
    name:String, 
    indexparams:Set[RateFXParameter], 
    ccy:String,
    discountCcy:String = "USD",
    discountSpread:Double = 0.00):IndexInitializer = {
    
    val params = indexparams.groupBy(_.instrument)
    if (!params.contains(yieldid) || !params.contains(spotid)) {return new EmptyInitializer}
    
    val yldparam:Map[qlPeriod, Double] = params(yieldid).map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
    
    val spot:Double = params(spotid).head.value
    
    val repo:Map[qlPeriod, Double] = params.get(repoid).collect{case rs => rs.map(p => (new qlPeriod(p.maturity), p.value)).toMap}.getOrElse(Map.empty)
    
    val vol:Map[qlPeriod, Double] = params.get(volid).collect{case vs => vs.map(p => (new qlPeriod(p.maturity), p.value)).toMap}.getOrElse(Map.empty)
     
    new IndexATMContinuous(name, ccy, spot, yldparam, repo, vol, discountCcy, discountSpread)
  }

}



//
//case class IndexATMContinuous(
//    name:String, 
//    indexparams:Set[RateFXParameter], 
//    ccy:String,
//    discountCcy:String = "USD",
//    discountSpread:Double = 0.00
//    ) extends IndexInitializer {
//  
//  val yieldid = "Yield"
//  val spotid = "Index"
//  val volid = "IndexVol"
//  val repoid = "Repo"
//  
//  override def getModel(market:Market):Option[Index] = {
//    val params = indexparams.groupBy(_.instrument)
//    if (!params.contains(yieldid) || !params.contains(spotid)) {return None}
//    
//    val valuedate = market.valuedate
//    val yldparam:Map[qlPeriod, Double] = params(yieldid).map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
//    val dividend = DividendCurve(valuedate, yldparam).orNull
//    if (dividend == null) {return None}
//    
//    val spot:Double = params(spotid).head.value
//    
//    val ratecurve = market.getDiscountCurve(ccy, discountCcy, discountSpread).orNull
//    if (ratecurve == null) {return None}
//    
//    val repo = (params.get(repoid) match {
//      case Some(rs) => 
//        val repoparam:Map[qlPeriod, Double] = rs.map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
//        RepoCurve(valuedate, repoparam)
//      case None => None
//    }).getOrElse(RepoCurve.zeroCurve(valuedate))
//    
//    val vol:YieldParameter = (params.get(volid) match {
//      case Some(vols) => YieldParameter(valuedate, vols.map(p => (new qlPeriod(p.maturity), p.value)).toMap)
//      case None => None
//    }).getOrElse(YieldParameter(valuedate, Double.NaN).get)
//     
//    Some(SmoothIndex(name, spot, ratecurve, dividend, repo, vol))
//  }
//
//}
//
