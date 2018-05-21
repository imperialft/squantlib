package net.squantlib.model.index

import net.squantlib.model.market.Market
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}
import net.squantlib.math.volatility.DupireLocalVolatility

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


case class IndexSmiledContinuous(
    name:String, 
    ccy:String,
    spot:Double,
    divYield:Map[qlPeriod, Double],
    repo:Map[qlPeriod, Double],
    atmVol: Map[qlPeriod, Double],
    smiledVol:Map[(qlPeriod, Double), Double],
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

    val atmVolCurve:YieldParameter = if (atmVol.isEmpty) YieldParameter(valuedate, Double.NaN).get else YieldParameter(valuedate, atmVol).getOrElse(YieldParameter(valuedate, Double.NaN).get)
    
    val inputVols = smiledVol.map{case ((d, k), v) => ((valuedate.days(d).toDouble, k), v)}.toMap

    val smiledVolCurve:YieldParameter3D = YieldParameter3D.construct(valuedate, inputVols, true).orNull
    if (smiledVolCurve == null) {return None}
    
//    val samplePoints2:Map[(Double, Double), Double] = inputVols.map{case ((d, k), v) =>
//      ((d, k), smiledVolCurve(d * 1.01, k * 1.01))
//    }.filter{case ((d, k), v) => !v.isNaN}
    
    val localVol:DupireLocalVolatility = DupireLocalVolatility(smiledVolCurve, ratecurve, dividend, spot)
    
    val samplePoints:Map[(Double, Double), Double] = inputVols.map{case ((d, k), v) =>
      ((d, k), localVol.localVolatility(d, k))
    }.filter{case ((d, k), v) => !v.isNaN}

    val filteredSample = if (samplePoints.size > 0) {
      val sampleAv = samplePoints.values.sum / samplePoints.size.toDouble
      samplePoints.filter{case ((d, k), v) => v >= sampleAv / 3.0 && v <= sampleAv * 3.0}
    } else samplePoints
    
    val localVolSurface = YieldParameter3D.construct(valuedate, filteredSample).orNull
    if (localVolSurface == null) {return None}
    
    if (filteredSample.size > Math.max(smiledVol.size * 0.5, 5)) {
      Some(SmoothIndex(name, spot, ratecurve, dividend, repoCurve, atmVolCurve, true, smiledVolCurve, localVolSurface))
    } else {
      Some(SmoothIndex(name, spot, ratecurve, dividend, repoCurve, atmVolCurve))
    }

  }

  override def mult(x:Double):IndexInitializer = IndexSmiledContinuous(
    name, 
    ccy,
    spot * x,
    divYield,
    repo,
    atmVol,
    smiledVol,
    discountCcy,
    discountSpread
  )
  
  override def addVol(x:Double):IndexInitializer = IndexSmiledContinuous(
    name, 
    ccy,
    spot,
    divYield,
    repo,
    atmVol.map{case (d, v) => (d, v+x)},
    smiledVol.map{case ((t, k), v) => ((t, k), v+x)},
    discountCcy,
    discountSpread
  )

  override def addDividend(x:Double):IndexInitializer = IndexSmiledContinuous(
    name, 
    ccy,
    spot,
    divYield.map{case (t, v) => (t, v+x)},
    repo,
    atmVol,
    smiledVol,
    discountCcy,
    discountSpread
  )
}

