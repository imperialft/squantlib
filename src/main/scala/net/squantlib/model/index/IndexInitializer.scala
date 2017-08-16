package net.squantlib.model.index

import net.squantlib.model.market.Market
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Period => qlPeriod}
import org.jquantlib.termstructures.{BlackVolatilityTermStructure, BlackVolTermStructure}
import org.jquantlib.termstructures.yieldcurves.FlatForward
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.termstructures.volatilities.LocalVolSurface
import net.squantlib.util.DisplayUtils._
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}

/**
 * Index specific discount curve calibration.
 */
trait IndexInitializer {
  
  def getModel(market:Market):Option[Index]
  
  def mult(x:Double):IndexInitializer
  
  def addVol(x:Double):IndexInitializer
  
  def addDividend(x:Double):IndexInitializer
}


case class EmptyInitializer() extends IndexInitializer {
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
    
    Some(SmoothIndex(name, spot, ratecurve, dividend, repoCurve, volCurve, false))
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


case class IndexSmiledContinuous(
    name:String, 
    ccy:String,
    spot:Double,
    divYield:Map[qlPeriod, Double],
    repo:Map[qlPeriod, Double],
    vol:Map[(qlPeriod, Double), Double],
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
    
    val volCurve:YieldParameter3D = YieldParameter3D(valuedate, vol.map{case ((d, k), v) => ((valuedate.days(d).toDouble, k), v)}.toMap)

    val indexVolTermStructure = IndexVolTermStructure(
      vd = valuedate.ql,
      minStrike = spot * 0.05,
      maxStrike = spot * 2.0,
      vol = (k, v) => volCurve(k, v),
      maxDate = valuedate.addMonths(12 * 10).ql
    )
  
    val simpleRateTs = new FlatForward(valuedate.ql, ratecurve.impliedRate(30.0), new Actual365Fixed)
  
    val simpleDivTs = new FlatForward(valuedate.ql, dividend(360), new Actual365Fixed)
  
    val localVolSurface:Option[LocalVolSurface] = try {
      Some(new LocalVolSurface(indexVolTermStructure, simpleRateTs, simpleDivTs, spot))
     } catch {case e:Throwable => 
      val errormsg = e.getStackTrace.mkString(sys.props("line.separator"))
      errorOutput(name, s"index calibration error - ${errormsg}")
      None
    }
     
    localVolSurface match {
      case Some(volsurf) => 
        val samplePoints:Map[(Double, Double), Double] = vol.map{case ((d, k), v) => 
          val dv = valuedate.days(d).toDouble
          try{
            Some(((dv, k), volsurf.localVol(dv / 365.25, k)))
          } catch {case e:Throwable => None}
        }.flatMap{case s => s}.toMap
        if (samplePoints.size > Math.max(vol.size * 0.7, 5) && samplePoints.values.forall(!_.isNaN)) {
          samplePoints.foreach(println)
          Some(SmoothIndex(name, spot, ratecurve, dividend, repoCurve, (k, v) => volCurve(k, v), true, YieldParameter3D(valuedate, samplePoints)))
        } else None
        
      case _ => None
    }
    
  }
  
  override def mult(x:Double):IndexInitializer = IndexSmiledContinuous(
    name, 
    ccy,
    spot * x,
    divYield,
    repo,
    vol,
    discountCcy,
    discountSpread
  )
  
  override def addVol(x:Double):IndexInitializer = IndexSmiledContinuous(
    name, 
    ccy,
    spot,
    divYield,
    repo,
    vol.map{case ((t, k), v) => ((t, k), v+x)},
    discountCcy,
    discountSpread
  )

  override def addDividend(x:Double):IndexInitializer = IndexSmiledContinuous(
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

case class IndexVolTermStructure (
    vd: qlDate, 
    override val minStrike:Double,
    override val maxStrike:Double,
    vol: (Double, Double) => Double,
    override val maxDate: qlDate
  ) extends BlackVolatilityTermStructure(vd) {
  
  override val dayCounter = new Actual365Fixed
  
  override def blackVolImpl(maturity:Double, strike:Double):Double = vol(maturity * 365.25, strike)
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
