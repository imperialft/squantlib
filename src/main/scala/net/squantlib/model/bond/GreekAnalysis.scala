package net.squantlib.model.bond

import net.squantlib.util.Date
import net.squantlib.util.{SimpleCache, FormulaParser}
import net.squantlib.model.market.Market
import scala.collection.breakOut
import net.squantlib.util.{MultiOrderNumber, MultiOrderMap, MultiOrderNumber2D, MultiOrderMap2D}
import net.squantlib.util.DisplayUtils._


trait GreekAnalysis {
  
  self : PriceableBond => 
    
  def greek(
      target:PriceableBond => Option[Double], 
      operation:Market => Option[Market],
      calculatedInitialPrice:Option[Double] = None
  ):Option[Double] = { 

    (market, getInitialPrice(target, calculatedInitialPrice)) match { 
      case (Some(mkt), Some(initialPrice)) => 
        operation(mkt) match {
          case Some(newMarket) => GreekAnalysis.greekCompute(getUncalibratedBond, target, newMarket, initialPrice)
          case _ => None
        }
      case _ => None
    }
  }

  def greekNewMarket(
      target:PriceableBond => Option[Double], 
      newMarket: Market,
      initialPrice:Double
  ):Option[Double] = GreekAnalysis.greekCompute(this, target, newMarket, initialPrice)
  
  def greekSecond(
      target:PriceableBond => Option[Double], 
      operation: Double => (Market => Option[Market]),
      shiftUp:Double, 
      shiftDown:Double,
      calculatedInitialPrice:Option[Double] = None
  ):MultiOrderNumber = {
    
    (market, getInitialPrice(target, calculatedInitialPrice)) match { 
      case (Some(mkt), Some(initialPrice)) =>
        (operation(shiftUp)(mkt), operation(shiftDown)(mkt)) match {
          case (Some(marketUp), Some(marketDown)) => GreekAnalysis.greekSecondCompute(getUncalibratedBond, target, shiftUp, marketUp, shiftDown, marketDown, initialPrice)
          case _ => MultiOrderNumber.empty
        }
        
      case _ => MultiOrderNumber.empty
    }
  }

  def greekSecondNewMarket(
      target:PriceableBond => Option[Double], 
      shiftUp:Double,
      marketUp:Market, 
      shiftDown:Double,
      marketDown:Market,
      initialPrice:Double
  ):MultiOrderNumber = GreekAnalysis.greekSecondCompute(this, target, shiftUp, marketUp, shiftDown, marketDown, initialPrice)


  def greekMultiOp (
      target:PriceableBond => Option[Double], 
      operation1: Double => (Market => Option[Market]),
      shift1:(Double, Double), 
      operation2: Double => (Market => Option[Market]),
      shift2:(Double, Double),
      calculatedInitialPrice:Option[Double] = None,
      editableBond:Boolean = false
  ):MultiOrderNumber2D  = (market, getInitialPrice(target, calculatedInitialPrice)) match { 
    case (Some(mkt), Some(initialPrice)) =>
      GreekAnalysis.greekMultiOpCompute(
        if (editableBond) this else getUncalibratedBond,
        mkt,
        target,
        operation1,
        shift1,
        operation2,
        shift2,
        initialPrice
      )
    case _ => MultiOrderNumber2D.empty
  }

  def greekMultiOp (
      target:PriceableBond => Option[Double], 
      mkt: Market,
      operation1: Double => (Market => Option[Market]),
      shift1:(Double, Double), 
      operation2: Double => (Market => Option[Market]),
      shift2:(Double, Double),
      initialPrice:Double,
      editableBond:Boolean
  ):MultiOrderNumber2D  = {
      GreekAnalysis.greekMultiOpCompute(
        if (editableBond) this else getUncalibratedBond,
        mkt,
        target,
        operation1,
        shift1,
        operation2,
        shift2,
        initialPrice
      )
  }
  
    
//  def greekMultiOp (
//      target:PriceableBond => Option[Double], 
//      operation1: Double => (Market => Option[Market]),
//      shift1:(Double, Double), 
//      operation2: Double => (Market => Option[Market]),
//      shift2:(Double, Double),
//      calculatedInitialPrice:Option[Double] = None
//  ):MultiOrderNumber2D  = {
//    
//    (market, getInitialPrice(target, calculatedInitialPrice)) match { 
//      case (Some(mkt), Some(initialPrice)) =>
//        
//        val s1 = shift1 match {
//          case (shiftUp, shiftDown) => 
//            (operation1(shiftUp)(mkt), operation1(shiftDown)(mkt)) match {
//              case (Some(marketUp), Some(marketDown)) => Some((shiftUp, marketUp), (shiftDown, marketDown))
//              case _ => None
//            }
//        }
//
//        val s2 = shift2 match {
//          case (shiftUp, shiftDown) => 
//            (operation2(shiftUp)(mkt), operation2(shiftDown)(mkt)) match {
//              case (Some(marketUp), Some(marketDown)) => Some((shiftUp, marketUp), (shiftDown, marketDown))
//              case _ => None
//            }
//        }
//        
//        val s12 = (shift1, shift2) match {
//          case ((shiftUp, shiftDown), (shiftUp2, _)) => 
//            (operation1(shiftUp)(mkt).flatMap{case m => operation2(shiftUp2)(m)}, operation1(shiftDown)(mkt).flatMap{case m => operation2(shiftUp2)(m)}) match {
//              case (Some(marketUp), Some(marketDown)) => Some(marketUp, marketDown)
//              case _ => None
//            }
//        }
//        
//        (s1, s2, s12) match {
//          case (Some(a), Some(b), Some(c)) => GreekAnalysis.greekMultiOpCompute(getUncalibratedBond, target, a, b, c, initialPrice)
//          case _ => MultiOrderNumber2D.empty
//        }
//        
//        
//      case _ => MultiOrderNumber2D.empty
//    }
//  }
//
//  def greekMultiOpNewMarket(
//      target:PriceableBond => Option[Double], 
//      shift1: ((Double, Market), (Double, Market)),
//      shift2: ((Double, Market), (Double, Market)),
//      shift12: (Market, Market),
//      initialPrice:Double
//  ):MultiOrderNumber2D = GreekAnalysis.greekMultiOpCompute(this, target, shift1, shift2, shift12, initialPrice)
  

  def getUncalibratedBond = {
    val b = this.copy
    b.modelCalibrated = true
    b.requiresCalibration = false
    b
  }
  
  private def getInitialPrice(target:PriceableBond => Option[Double], initialPrice:Option[Double]) = initialPrice match {
    case Some(p) => Some(p)
    case None => target(this)
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
    greekSecond((b:PriceableBond) => b.modelPriceJpy, (s:Double) => ((m:Market) => m.underlyingShifted(id, (s + 1.0))), shiftUp - 1.0, shiftDown - 1.0)
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

  /*  
   * Returns vanna for any underlying
   */
  def underlyingVanna(id:String, spotShiftUp:Double, spotShiftDown:Double, volShiftUp:Double, volShiftDown:Double):MultiOrderNumber2D = {
    greekMultiOp (
      target = (b:PriceableBond) => b.modelPriceJpy,
      operation1 = (s:Double) => ((m:Market) => m.underlyingShifted(id, (s + 1.0))),
      shift1 = (spotShiftUp - 1.0, spotShiftDown - 1.0),
      operation2 = (s:Double) => ((m:Market) => m.underlyingVolShifted(id, s)),
      shift2 = (volShiftUp, volShiftDown),
      calculatedInitialPrice = None,
      editableBond = false
    )
  }
  
  def underlyingVannas(spotShiftUp:Double, spotShiftDown:Double, volShiftUp:Double, volShiftDown:Double):MultiOrderMap2D = {
    val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
    val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
    MultiOrderMap2D(uls.map(ul => (ul -> underlyingVanna(ul, spotShiftUp, spotShiftDown, volShiftUp, volShiftDown))))
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
      val shifts:Map[String, Double] = c.triggers.getDouble.map{case (k, v) => mkt.getUnderlying(k) match {
        case Some(ul) if ul.spot > 0.0 => (k, v / ul.spot)
        case _ => (k, Double.NaN)
      }}
      if (shifts.exists{case (k, v) => v.isNaN}) None
      else mkt.underlyingShifted(shifts) match {
        case Some(newmkt) => 
          dateshifted.market = newmkt
          val fwdPnl:Double = if (c.isForward) c.forward.getDouble.map{case (k, v) => mkt.getUnderlying(k) match {
            case Some(ul) if ul.spot > 0.0 => ul.spot / v
            case _ => Double.NaN
          }}.min else 1.0
          
          dateshifted.dirtyPrice.collect{case p => (1.0 + c.bonusAmount.toDouble) * fwdPnl - p}
        case _ => None
      }
    case _ => None
  }

}

object GreekAnalysis {

  def greekCompute(
      bond: PriceableBond,
      target:PriceableBond => Option[Double], 
      newMarket: Market,
      initialPrice:Double
  ):Option[Double] = { 
    bond.setMarketNoCalibration(newMarket)
    target(bond) match { 
      case Some(n) if !n.isNaN && !n.isInfinity => Some(n - initialPrice) 
      case _ => None 
    }
  }
  
  def greekSecondCompute(
      bond: PriceableBond,
      target:PriceableBond => Option[Double], 
      shiftUp:Double,
      marketUp:Market, 
      shiftDown:Double,
      marketDown:Market,
      initialPrice:Double
  ):MultiOrderNumber = {
    
      def computePrice(newmkt:Market):Option[Double] = {
        bond.setMarketNoCalibration(newmkt)
        target(bond) match { 
          case Some(n) if !n.isNaN && !n.isInfinity => Some(n)
          case _ => None 
        }
      }

      val priceUp = computePrice(marketUp)
      val pos = priceUp.collect{case n => (n - initialPrice) / shiftUp}
      val priceDown = computePrice(marketDown)
      val neg = priceDown.collect{case n => (n - initialPrice) / shiftDown}

      val (delta, gamma) = (pos, neg) match {
        case (Some(p), Some(n)) => (Some((p + n) / 2.0), Some((p - n) / ((shiftUp - shiftDown) / 2.0)))
        case _ => (None, None)
      }

      MultiOrderNumber(Some(initialPrice), delta, gamma, None, priceUp, priceDown)
  }

  def greekSecondCompute(
      bond: PriceableBond,
      mkt: Market,
      target:PriceableBond => Option[Double], 
      operation: Double => (Market => Option[Market]),
      shiftUp:Double,
      shiftDown:Double,
      initialPrice:Double
  ):MultiOrderNumber = {

      def computePrice(s:Double):Option[Double] = {
        operation(s)(mkt) match {
          case Some(newmkt) =>
            bond.setMarketNoCalibration(newmkt)
            target(bond) match {
              case Some(n) if !n.isNaN && !n.isInfinity => Some(n)
              case _ => None 
            }
          case _ => None
        }
      }

      val priceUp = computePrice(shiftUp)
      val pos = priceUp.collect{case n => (n - initialPrice) / shiftUp}
      val priceDown = computePrice(shiftDown)
      val neg = priceDown.collect{case n => (n - initialPrice) / shiftDown}

      val (delta, gamma) = (pos, neg) match {
        case (Some(p), Some(n)) => (Some((p + n) / 2.0), Some((p - n) / ((shiftUp - shiftDown) / 2.0)))
        case _ => (None, None)
      }

      MultiOrderNumber(Some(initialPrice), delta, gamma, None, priceUp, priceDown)
  }
  
  def greekMultiOpCompute(
      bond: PriceableBond,
      mkt: Market, 
      target: PriceableBond => Option[Double], 
      operation1: Double => (Market => Option[Market]),
      shift1: (Double, Double),
      operation2: Double => (Market => Option[Market]),
      shift2: (Double, Double),
      initialPrice:Double
  ):MultiOrderNumber2D = {

    val greek1 = shift1 match {case (shiftUp, shiftDown) =>
      GreekAnalysis.greekSecondCompute(bond, mkt, target, operation1, shiftUp, shiftDown, initialPrice)
    }

    val greek2 = shift2 match {case (shiftUp, shiftDown) =>
      GreekAnalysis.greekSecondCompute(bond, mkt, target, operation2, shiftUp, shiftDown, initialPrice)
    }

    def computePrice(newmkt:Market, spotShift:Double, spotShiftedPrice:Double, volShift:Double, spotVega:Double):Option[Double] = {
      bond.setMarketNoCalibration(newmkt)
      target(bond) match {
        case Some(n) if !n.isNaN && !n.isInfinity => 
          val shiftedVega = (n - spotShiftedPrice) / volShift
          Some((shiftedVega - spotVega) / spotShift)
        case _ => None 
      }
    }
    
    val (shiftUp, shiftDown) = shift1
    val shiftV = shift2 match {case (s1, _) => s1}

    val (pos, neg) = (greek1.priceUp, greek1.priceDown, greek2.priceUp, greek2.spot) match {
      case (Some(pup), Some(pdown), Some(vpup), Some(vspot)) =>
        val spotVega = (vpup - vspot) / shiftV
        val p1 = operation1(shiftUp)(mkt).flatMap{case m => operation2(shiftV)(m)}.flatMap{case m => computePrice(m, shiftUp, pup, shiftV, spotVega)}
        val p2 = operation1(shiftDown)(mkt).flatMap{case m => operation2(shiftV)(m)}.flatMap{case m => computePrice(m, shiftUp, pup, shiftV, spotVega)}
        (p1, p2)
      case _ => (None, None)
    }
    
    val (delta, gamma) = (pos, neg) match {
      case (Some(p), Some(n)) => (Some((p + n) / 2.0), Some((p - n) / ((shiftUp - shiftDown) / 2.0)))
      case _ => (None, None)
    }

    val greek12 = MultiOrderNumber(greek2.firstOrder, delta, gamma, None, pos, neg)
    
    MultiOrderNumber2D(greek1, greek2, greek12)
  }
  
}