package net.squantlib.model.index

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import net.squantlib.util.Date

/**
 * Orthodox index with continuous dividend yield.
 */
case class SmoothIndex(
    id:String, 
    var spot:Double,
    rateCurve:DiscountCurve, 
    dividend:DividendCurve, 
    repo:RepoCurve, 
    atmVol: Double => Double,
    override val isSmiledVol:Boolean,
    smiledVolSurface: (Double, Double) => Double,
    localVolSurface: (Double, Double) => Double
  ) extends Index  {
  
	override val valuedate = rateCurve.valuedate
	
	override val discontinousDates = Set.empty[Date]
	
	override val latestPrice = Some(spot)
	
  override val dividends:Map[Double, Double] = Map.empty
  
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike index strike
	 */
	override def volatility(days:Double):Double = atmVol(days)

  override def smiledVolatility(days:Double, strike:Double):Double = smiledVolSurface(days, strike)
	
	override def localVolatility(days:Double, strike:Double):Double = if (localVolSurface == null) Double.NaN else localVolSurface(days, strike)
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double) : Double = spot * math.exp((rateCurve.impliedRate(days) - repo(days) - dividend(days)) * days / 365.25)

    override def repoRate(days:Double):Double = repo(days)
    
    override def assetYield(days:Double):Double = dividend(days)
    
    // override def expectedYield:Option[Double] = Some(dividendYield(360) - repoRate(360))
    
    // override def expectedCoupon:Option[Double] = expectedYield
    
    
} 

object SmoothIndex {

  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, atmVol:YieldParameter):SmoothIndex = {
    SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double) => atmVol(y), false, (d:Double, k:Double) => Double.NaN, (d:Double, k:Double) => Double.NaN)
  }

  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, atmVol:YieldParameter, isSmiledVol:Boolean, smiledVolSurface:YieldParameter3D, localVolSurface:YieldParameter3D):SmoothIndex = {
    if (isSmiledVol) SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double) => atmVol(y), true, (d:Double, k:Double) => smiledVolSurface(d, k), (d:Double, k:Double) => localVolSurface(d, k))
    else apply(name, spot, rateCurve, dividend, repo, atmVol)
  }
  
//  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, vol:YieldParameter3D, isSmiledVol:Boolean):SmoothIndex = 
//    SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double) => vol(y, spot), true, )
    
}

