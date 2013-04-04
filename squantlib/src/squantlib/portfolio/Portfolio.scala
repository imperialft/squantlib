package squantlib.portfolio

import squantlib.model.StaticAsset
import collection.mutable.{Map => MutableMap, WeakHashMap}
import squantlib.util.DisplayUtils._

case class Portfolio(assets:Map[StaticAsset, Double]) extends Map[StaticAsset, Double] {

  val defaultNbDays = 260
  
  val weights = assets.mapValues(v => v / assets.values.sum)
  
  val priceableWeights = {
    val validassets = assets.filter(_._1.historicalVolLatest(defaultNbDays).isDefined)
    validassets.mapValues(v => v / validassets.values.sum)
  }
  
  override def +[B>:Double](kv:(StaticAsset, B)):Map[StaticAsset, B] = assets + kv
  
  override def -(k:StaticAsset):Map[StaticAsset, Double] = assets - k
  
  override def get(k:StaticAsset):Option[Double] = assets.get(k)

  override def iterator = assets.iterator
  
  override def contains(b:StaticAsset):Boolean = assets.contains(b)
  
  val correlationCache = WeakHashMap.empty[Int, SymmetricMatrix[StaticAsset]]
  
  def correlationMatrix(nbDays:Int = defaultNbDays):SymmetricMatrix[StaticAsset] = correlationCache.getOrElseUpdate(nbDays, 
    SymmetricMatrix(priceableWeights.keySet, (a:StaticAsset, b:StaticAsset) => a.historicalCorrelLatest(b, nbDays).getOrElse(0.0)))
    
  def correlation(a:StaticAsset, b:StaticAsset, nbDays:Int = defaultNbDays):Double = correlationMatrix(nbDays).get(a, b)
    
  val covarianceCache = WeakHashMap.empty[Int, SymmetricMatrix[StaticAsset]]
  
  def covarianceMatrix(nbDays:Int = defaultNbDays):SymmetricMatrix[StaticAsset] = covarianceCache.getOrElseUpdate(nbDays, 
    SymmetricMatrix(priceableWeights.keySet, (a:StaticAsset, b:StaticAsset) => 
      priceableWeights(a) * priceableWeights(b) * a.historicalVolLatest(nbDays).getOrElse(0.0) * b.historicalVolLatest(nbDays).getOrElse(0.0) * correlation(a, b, nbDays)))
  
  def covariance(a:StaticAsset, b:StaticAsset, nbDays:Int = defaultNbDays):Double = covarianceMatrix(nbDays).get(a, b)
  
  def volatilityNoCorrel(nbDays:Int = defaultNbDays):Double = math.sqrt(priceableWeights.map{case (a, w) => a.historicalVolLatest(nbDays) match {
    case Some(v) => v * v * w * w
    case _ => 0.0
  }}.sum)
  
  def volatility:Double = volatility(defaultNbDays)
  
  def volatility(nbDays:Int):Double = math.sqrt(covarianceMatrix(nbDays).sum(true))
  
  def expectedYield:Double = priceableWeights.map{case (a, w) => a.expectedYield.getOrElse(0.0) * w}.sum
  
  def expectedCoupon:Double = priceableWeights.map{case (a, w) => a.expectedCoupon.getOrElse(0.0) * w}.sum
  
  def sharpeRatio:Double = sharpeRatio(defaultNbDays)
  
  def sharpeRatio(nbDays:Int):Double = expectedYield / volatility(nbDays)
  
  def assetSelect[A<:StaticAsset](candidates:Set[A], conditions:List[Portfolio => Double], addRatio:Double, nbResult:Int):List[List[(String, Double)]] = {
    val performances:List[List[(String, Double)]] = candidates.map(c => {
      val newportfolio = Portfolio(weights + (c -> addRatio))
      val perflist = conditions.map(v => (c.id, v(newportfolio)))
      println("try " + c.id + " x " + addRatio.asPercent(0) + " => " + perflist.map(_._2.asPercent(3)).mkString(", "))
      perflist
    }) (collection.breakOut)
    performances.transpose.map(_.sortBy(_._2).reverse take nbResult)
  }
  
  def show = {
    println("Asset\t\tPrice\tWeight\tCoupon\tYield\tVolatility")
    assets.foreach{case (a, w) => println(
        a.id + "\t" + 
        a.latestPrice.collect{case v => v.asPercent(2)}.getOrElse("N/A") + "\t" + 
        weights(a).asPercent(2) + "\t" + 
        a.expectedCoupon.collect{case v => v.asPercent(2)}.getOrElse("N/A") + "\t" + 
        a.expectedYield.collect{case v => v.asPercent(2)}.getOrElse("N/A") + "\t" + 
        a.historicalVolLatest(defaultNbDays).collect{case v => v.asPercent(2)}.getOrElse("N/A")
    )}
    
    println("Correlation Matrix")
    correlationMatrix().show
    
    println("Covariance Matrix")
    covarianceMatrix().show
    
    println("Portfolio Analysis")
    println("Volatility: "  + volatility.asPercent(2))
    println("Volatility(0% correl) : "  + volatilityNoCorrel().asPercent(2))
    println("Coupon: "  + expectedCoupon.asPercent(2))
    println("Yield : "  + expectedYield.asPercent(2))
    println("SharpeRatio: "  + sharpeRatio.asDouble(4))
  }

}

case class SymmetricMatrix[A <: AnyRef{val id:String}](elements:Set[A], operator:(A, A) => Double) {
  
  val cachedValues = MutableMap.empty[(A, A), Double]
  
  def get(a:A, b:A):Double = {
    assert ((elements contains a) && (elements contains b))
    if (cachedValues contains (a, b)) cachedValues((a, b))
    else if (cachedValues contains (b, a)) cachedValues((b, a))
    else {
      cachedValues((a, b)) = operator(a, b)
      cachedValues((a, b))
    }
  }
  
  def sum(includeDiagonal:Boolean):Double =
    if (includeDiagonal) (for (a <- elements; b <- elements) yield get(a, b)).sum
    else (for (a <- elements; b <- elements - a) yield get(a, b)).sum
  
  def show:Unit = {
    val elementList = elements.toList
    elementList.foreach(a => println(a.id + "\t" + elementList.map(get(a, _).asPercent(2)).mkString("\t")))
  }
  
}