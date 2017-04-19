package net.squantlib.model.bond

import net.squantlib.util.Date
import net.squantlib.util.{SimpleCache, FormulaParser}
import net.squantlib.model.market.Market
import scala.collection.breakOut
import net.squantlib.util.{MultiOrderNumber, MultiOrderMap}
import net.squantlib.util.DisplayUtils._


trait GreekAnalysis {
  
  self : PriceableBond => 
  
  def greek(
      target:PriceableBond => Option[Double], 
      operation:Market => Option[Market],
      initialPrice:Option[Double] = None,
      editableBond:Boolean = false
  ):Option[Double] = market.flatMap { case mkt =>
    
    val initprice = initialPrice match {
      case Some(p) => Some(p)
      case None => target(this)
    }
    
    val newBond = if (editableBond) this else {
      val b = this.copy
      b.modelCalibrated = true
      b.requiresCalibration = false
      b
    }
    
    operation(mkt) match {
      case Some(newmkt) =>
        newBond.setMarketNoCalibration(newmkt)
        (initprice, target(newBond)) match { 
          case (Some(i), Some(n)) if !i.isNaN && !n.isNaN && !i.isInfinity && !n.isInfinity => Some(n - i) 
          case _ => None 
        }
      case _ => None
    }
  }

  def greekNewMarket(
      target:PriceableBond => Option[Double], 
      newMarket: Market,
      initialPrice:Double
  ):Option[Double] = market.flatMap { case mkt =>
    
    setMarketNoCalibration(newMarket)
    target(this) match { 
      case Some(n) if !n.isNaN && !n.isInfinity => Some(n - initialPrice) 
      case _ => None 
    }
  }
  
  def greekSecond(
      target:PriceableBond => Option[Double], 
      operation: Double => (Market => Option[Market]),
      shiftUp:Double, 
      shiftDown:Double,
      initialPrice:Option[Double] = None,
      editableBond:Boolean = false
  ):MultiOrderNumber = market match { 
    case None => MultiOrderNumber.empty

    case Some(mkt) =>
      val initprice = initialPrice match {
        case Some(p) => Some(p)
        case None => target(this)
      }
      
      val newBond = if (editableBond) this else {
        val b = this.copy
        b.modelCalibrated = true
        b.requiresCalibration = false
        b
      }
      
      def computeDelta(s:Double):Option[Double] = {
        val newmkt = operation(s)(mkt).orNull
        if (newmkt == null) {return None}
        newBond.setMarketNoCalibration(newmkt)
        val newprice = target(newBond)
        (initprice, newprice) match { 
          case (Some(i), Some(n)) if !i.isNaN && !n.isNaN && !i.isInfinity && !n.isInfinity => Some((n - i) / s)
          case _ => None 
        }
      }

      val pos = computeDelta(shiftUp)
      val neg = computeDelta(shiftDown)
      
      val (delta, gamma) = (pos, neg) match {
        case (Some(p), Some(n)) => (Some((p + n) / 2.0), Some((p - n) / ((shiftUp - shiftDown) / 2.0)))
        case _ => (None, None)
      }

      MultiOrderNumber(delta, gamma)
  }

  def greekSecondNewMarket(
      target:PriceableBond => Option[Double], 
      shiftUp:Double,
      marketUp:Market, 
      shiftDown:Double,
      marketDown:Market,
      initialPrice:Double
  ):MultiOrderNumber = {
    
      def computePrice(newmkt:Market, shift:Double):Option[Double] = {
        setMarketNoCalibration(newmkt)
        target(this) match { 
          case Some(n) if !n.isNaN && !n.isInfinity => Some((n - initialPrice) / shift)
          case _ => None 
        }
      }

      val pos = computePrice(marketUp, shiftUp)
      println(s"delta positive ${pos.toString}")
      val neg = computePrice(marketDown, shiftDown)
      println(s"delta negative ${neg.toString}")
      
      val (delta, gamma) = (pos, neg) match {
        case (Some(p), Some(n)) => (Some((p + n) / 2.0), Some((p - n) / ((shiftUp - shiftDown) / 2.0)))
        case _ => (None, None)
      }

      println(s"delta result ${delta.toString}")
      println(s"gamma result ${gamma.toString}")
      
      MultiOrderNumber(delta, gamma)
  }
  
  /*  
   * Returns rate delta
   */
  def rateDelta(shift:Double):Option[Double] = rateDelta(currency.code, shift)
  
  def rateDelta(ccy:String, shift:Double):Option[Double] = rateDelta((b:PriceableBond) => b.modelPrice, Map(ccy -> shift))
    
  def rateDelta(target:PriceableBond => Option[Double], shift:Map[String, Double]):Option[Double] = greek(target, (m:Market) => Some(m.rateShifted(shift)))

  
  /*  
   * Returns rate delta for all involved currencies.
   */
  def rateDeltas(shift:Double):Map[String, Double] = currencyList.map(f => (f, rateDelta(f, shift))).collect{case (a, Some(b)) => (a, b)} (breakOut)
  
  /*  
   * Returns effective duration defined as 1bp rate delta * 10000
   */
  def effectiveDuration:Option[Double] = if (isTerminated == Some(true)) Some(0.0) else rateDelta(-0.0001).collect{case d => d * 10000} // TO BE COMPUTED AS RATE DELTA
  

  /*  
   * Return FX delta defined as MtM change when multiplying FX by given amount
   */
  def fxDelta(ccy:String, mult:Double):Option[Double] = fxDelta((b:PriceableBond) => b.modelPrice, Map(ccy -> mult))
  
  def fxDelta(target:PriceableBond => Option[Double], mult:Map[String, Double]):Option[Double]  = greek(target, (m:Market) => Some(m.fxShifted(mult)))
  
  /*  
   * Returns FX delta for all involved currencies.
   */
  def fxDeltas(mult:Double):Map[String, Double] = (currencyList - currency.code).map(ccy => ("USD" + ccy, fxDelta((b:PriceableBond) => b.modelPrice, Map(ccy -> mult)))).collect{case (a, Some(b)) => (a, b)}.toMap
    
  /*  
   * Returns FX delta on JPY bond price.
   */
  def fxDeltaJpy(mult:Double):Map[String, Double] = (currencyList - "JPY").map(f => 
    (f + "JPY", fxDelta((b:PriceableBond) => b.modelPriceJpy, Map(f -> 1/mult)))).collect{case (a, Some(b)) => (a, b)} (breakOut)

    
  /*  
   * Returns delta of 1 yen change in FX on JPY price.
   */
  def fxDeltaOneJpy:Map[String, Double] = market match {
    case None => Map.empty
    case Some(mkt) => (currencyList - "JPY").map(ccy => mkt.fx(ccy, "JPY") match {
        case Some(fx) => (ccy + "JPY", fxDelta((b:PriceableBond) => b.modelPriceJpy, Map(ccy -> fx/(fx+1))))
        case None => (ccy + "JPY", None)
      }).collect{case (a, Some(b)) => (a, b)}(breakOut)}
      
  
  /*  
   * Returns rate vega
   */
  def fxVegas(addvol:Double):Map[String, Double] = fxList.map(fx => (fx, fxVega(fx, addvol))).collect{case (a, Some(b)) => (a, b)} (breakOut)
  
  def fxVega(ccypair:String, addvol:Double):Option[Double] = fxVega((b:PriceableBond) => b.modelPrice, Map(ccypair -> addvol))
  
  def fxVega(target:PriceableBond => Option[Double], addvol:Map[String, Double]):Option[Double] = greek(target, (m:Market) => Some(m.fxVolShifted(addvol)))
    
  
  /*  
   * Returns delta for any underlying
   */
  def underlyingDelta(id:String, shift:Double):Option[Double] = greek((b:PriceableBond) => b.modelPriceJpy, (m:Market) => m.underlyingShifted(id, shift))
  
  def underlyingDeltas(shift:Double):Map[String, Option[Double]] = {
    val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
    val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
    uls.map(ul => (ul, underlyingDelta(ul, shift)))(collection.breakOut)
  }

  /*  
   * Returns gamma for any underlying
   */
  def underlyingGamma(id:String, shiftUp:Double, shiftDown:Double):MultiOrderNumber = {
    greekSecond((b:PriceableBond) => b.modelPriceJpy, (s:Double) => ((m:Market) => m.underlyingShifted(id, s)), shiftUp, shiftDown)
  }

  def underlyingGammas(shiftUp:Double, shiftDown:Double):MultiOrderMap = {
    val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
    val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
    MultiOrderMap(uls.map(ul => (ul -> underlyingGamma(ul, shiftUp, shiftDown))))
  }
  
  /*  
   * Returns vega for any underlying
   */
  def underlyingVega(id:String, shift:Double):Option[Double] = greek((b:PriceableBond) => b.modelPriceJpy, (m:Market) => m.underlyingVolShifted(id, shift))
  
  def underlyingVegas(shift:Double):Map[String, Option[Double]] = {
    val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
    val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
    uls.map(ul => (ul, underlyingVega(ul, shift))) (collection.breakOut)
  }

  /*  
   * Returns gamma vol for any underlying
   */
  def underlyingVolGamma(id:String, shiftUp:Double, shiftDown:Double):MultiOrderNumber = {
    greekSecond((b:PriceableBond) => b.modelPriceJpy, (s:Double) => ((m:Market) => m.underlyingVolShifted(id, s)), shiftUp, shiftDown)
  }
  
  def underlyingVolGammas(shiftUp:Double, shiftDown:Double):MultiOrderMap = {
    val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
    val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
    MultiOrderMap(uls.map(ul => (ul -> underlyingVolGamma(ul, shiftUp, shiftDown))))
  }
  
  def effectiveConvexity(shift:Double):Option[Double] = {
    val durationlow = rateDelta(-shift)
    val durationhigh = rateDelta(shift)
    (durationlow, durationhigh) match {
      case (Some(l), Some(h)) => Some((l + h) / shift)
      case _ => None
    }
  }
  
  def convexity:Option[Double] = effectiveConvexity(0.0001)
  
  /*
   * Trigger Probability Analysis
   */
  /*  
   * Returns delta of 1 yen change in FX on JPY price.
   */
  def triggerProbabilities:List[Double] = model match {
    case Some(m) => m.triggerProbabilities
    case _ => List.empty
  }
  
  def updateTriggerProbabilities:Unit = model match {
    case Some(m) => m.updateTriggerProbabilities
    case _ => List.empty
  }
  
  def nextTriggerBinarySize:Option[Double] = (market, livePayoffs.headOption) match {
    case (Some(mkt), Some((s, p, c))) if c.isTrigger => 
      val dateshifted = this.dateShifted(mkt.valuedate.sub(s.paymentDate).toInt)
      val shifts = c.triggers.map{case (k, v) => mkt.getUnderlying(k) match {
        case Some(ul) if ul.spot > 0.0 => (k, v / ul.spot)
        case _ => (k, Double.NaN)
      }}
      if (shifts.exists{case (k, v) => v.isNaN}) None
      else mkt.underlyingShifted(shifts) match {
        case Some(newmkt) => 
          dateshifted.market = newmkt
          val fwdPnl = if (c.isForward) c.forward.map{case (k, v) => mkt.getUnderlying(k) match {
            case Some(ul) if ul.spot > 0.0 => ul.spot / v
            case _ => Double.NaN
          }}.min else 1.0
          
          dateshifted.dirtyPrice.collect{case p => (1.0 + c.bonusAmount) * fwdPnl - p}
        case _ => None
      }
    case _ => None
  }

}