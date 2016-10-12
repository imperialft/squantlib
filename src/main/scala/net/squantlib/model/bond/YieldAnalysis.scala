package net.squantlib.model.bond

import net.squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod, TimeUnit, Calendar, Frequency}
import org.jquantlib.termstructures.Compounding
import org.jquantlib.daycounters.{Actual365Fixed, DayCounter}
import net.squantlib.util.initializer.Currencies
import net.squantlib.util.{SimpleCache, FormulaParser}
import net.squantlib.math.financial.{BondYield, Duration}
import net.squantlib.model.market.Market
import net.squantlib.util.{UnderlyingParser, UnderlyingParsers}
import net.squantlib.schedule.CalculationPeriod
import scala.collection.breakOut
import scala.language.postfixOps

trait YieldAnalysis {
  
  self : PriceableBond => 

  /*  
   * Returns bond yield.
   * @param comp Compounding rate, as one of the following
   *     "None" => Discounting is not taken into account : ZC = 1.0
   *     "Simple" => No compounding : ZC = 1 / rt
   *     "Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 
   *     "Continuous" => 
   */
    
  def couponYield(dc:DayCounter):Option[Double] = {
    val cashflows = spotCashflowDayfrac(dc)
    doubleOption(accruedAmount.collect{case acc => (cashflows.unzip._2.sum - acc - 1.0) / cashflows.unzip._1.max})
  }
  
  private def doubleOption(d:Double):Option[Double] = if (d.isNaN || d.isInfinity) None else Some(d)
  private def doubleOption(d:Option[Double]):Option[Double] = d.flatMap{case v => doubleOption(v)}
  
  def getYield(comp:Compounding):Option[Double] = getYield(comp, Frequency.Annual)
  
  def getYield(comp:Compounding, freq:Frequency):Option[Double] = getYield(comp, freq, new Actual365Fixed, 0.00001, 20)
  
  def getYield(comp:Compounding, freq:Frequency, dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = 
    if (useCouponAsYield) couponYield(dc)
    else doubleOption(dirtyPrice.flatMap{case p => getYield(p, dc, comp, freq, accuracy, maxIteration)})
  
  def getYield(price:Double, comp:Compounding, freq:Frequency):Option[Double] = 
    getYield(price, new Actual365Fixed, comp, freq, 0.0001, 20)
  
  def getYield(price:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = 
    getYield(price, spotCashflowDayfrac(dc), comp, freq, accuracy, maxIteration)
    
  def getYield(
      price:Double, 
      cashflows:List[(Double, Double)], 
      comp:Compounding, freq:Frequency, 
      accuracy:Double, 
      maxIteration:Int):Option[Double] = {
    
    val result:Option[Double] = 
      if (cashflows isEmpty) None
      else if (useCouponAsYield) accruedAmount.collect{case acc => BondYield.asAverageCoupon(cashflows, acc)}
      else comp match {
        case Compounding.Simple => BondYield.solveNoCompounding(price, cashflows, accuracy, maxIteration)
        case Compounding.Compounded | Compounding.SimpleThenCompounded => BondYield.solveCompounded(price, cashflows, freq.toInteger, accuracy, maxIteration)
        case Compounding.Continuous => BondYield.solveContinuous(price, cashflows, accuracy, maxIteration)
        case Compounding.None => accruedAmount.collect{case acc => BondYield.solveNoRate(price, cashflows, acc)}
        case _ => None
     }
    doubleOption(result)
  }
  
  /*
   * Compute yield for the lifetime of the bond
   */
  
  def getLifetimeYield(comp:Compounding):Option[Double] = getLifetimeYield(comp, Frequency.Annual)
  
  def getLifetimeYield(comp:Compounding, freq:Frequency):Option[Double] = getLifetimeYield(comp, freq, new Actual365Fixed, 0.00001, 20)
  
  def getLifetimeYield(comp:Compounding, freq:Frequency, dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = 
    if (useCouponAsYield) couponYield(dc) 
    else doubleOption(getLifetimeYield(spotCashflowDayfracAllJpy(dc), comp, freq, accuracy, maxIteration))
  
  def getLifetimeYield(
      cashflows:List[(Double, Double)], 
      comp:Compounding, 
      freq:Frequency, 
      accuracy:Double, 
      maxIteration:Int):Option[Double] = {
    
    val result:Option[Double] = 
      if (cashflows isEmpty) None
      else if (useCouponAsYield) accruedAmount.collect{case acc => BondYield.asAverageCoupon(cashflows, acc)}
      else issuePrice.flatMap{case price => comp match {
        case Compounding.Simple => BondYield.solveNoCompounding(price / 100.0, cashflows, accuracy, maxIteration)
        case Compounding.Compounded | Compounding.SimpleThenCompounded => BondYield.solveCompounded(price/ 100.0, cashflows, freq.toInteger, accuracy, maxIteration)
        case Compounding.Continuous => BondYield.solveContinuous(price/ 100.0, cashflows, accuracy, maxIteration)
        case Compounding.None => accruedAmount.collect{case acc => BondYield.solveNoRate(price/ 100.0, cashflows, acc)}
        case _ => None
     }}
    doubleOption(result)
  }
  
   /*  
   * Continuous rate at which MtM exceeds 100% at next call date.
   */
  def nextRateFrontier:Option[Double] = nextBermudan.flatMap{ case d => getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20) }
  
  /*  
   *  Returns bond yield assuming purchase date is market value date, price is current dirty price.
   */
  def yieldNoCompounding:Option[Double] = getYield(Compounding.Simple)
  
  def yieldContinuous:Option[Double] = getYield(Compounding.Continuous)
  
  def yieldSemiannual:Option[Double] = getYield(Compounding.Compounded, Frequency.Semiannual)
  
  def yieldAnnual:Option[Double] = getYield(Compounding.Compounded, Frequency.Annual)
  
  def yieldSimple:Option[Double] = getYield(Compounding.None, Frequency.Annual)
  
  /*  
   *  Returns bond yield, assuming purchase date is issue date, price is issue date.
   */
  def lifetimeYieldNoCompounding:Option[Double] = getLifetimeYield(Compounding.Simple)
  
  def lifetimeYieldContinuous:Option[Double] = getLifetimeYield(Compounding.Continuous)
  
  def lifetimeYieldSemiannual:Option[Double] = getLifetimeYield(Compounding.Compounded, Frequency.Semiannual)
  
  def lifetimeYieldAnnual:Option[Double] = getLifetimeYield(Compounding.Compounded, Frequency.Annual)
  
  def lifetimeYieldSimple:Option[Double] = getLifetimeYield(Compounding.None, Frequency.Annual)
  
  /*  Returns yield at which bond price becomes 100% (if any)
   * @param comp Compounding rate, as one of the following
   *     "None" => Not applicable
   *     "Simple" => No compounding : ZC = 1 / rt
   *     "Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf   
   *     "Continuous" => 
   */
  def parMtMYield:Option[Double] = getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20)

  
  /*  
   * Internal Rate of Return, defined to be the same as annually compounded yield.
   */
  def irr:Option[Double] = irr(new Actual365Fixed, 0.00001, 20)
  def irr(dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = yieldAnnual
  
  
  /*  
   * Returns Macauley duration defined as Sum {tV} / Sum{V}
   */
  def macaulayDuration:Option[Double] = 
    if (isTerminated == Some(true)) Some(0.0) 
    else discountCurve.flatMap{
      case curve => doubleOption(Duration.macaulay(spotCashflowDayfrac(new Actual365Fixed), (d:Double) => curve(d * 365.25)))}
  
  /*  
   * Returns modified duration defined as Macauley duration / (1 + yield / freq)
   */
  def modifiedDuration:Option[Double] = modifiedDuration(Compounding.Compounded, Frequency.Annual)
  def modifiedDuration(comp:Compounding, freq:Frequency):Option[Double] = if (isTerminated == Some(true)) Some(0.0) 
    else macaulayDuration.flatMap { case dur =>
      comp match {
        case Compounding.Continuous => Some(dur)
        case Compounding.Compounded | Compounding.SimpleThenCompounded => getYield(comp, freq).collect{case y => dur / (1.0 + y / freq.toInteger.toDouble)}
        case _ => None
    }
  }
  
  /*
   * Yield value of a basis point. The yield value of a one basis point change
   * in price is the derivative of the yield with respect to the price
   */
  def yieldValueBasisPoint:Option[Double] = (dirtyPrice, modifiedDuration) match {
    case (Some(p), Some(dur)) => Some(1.0 / (-p * dur))
    case _ => None
  }
  
  /*
     * Cash-flow convexity
     * The convexity of a string of cash flows is defined as {@latex[ C = \frac{1}{P} \frac{\partial^2 P}{\partial y^2} } where
     * {@latex$ P } is the present value of the cash flows according to the given IRR {@latex$ y }.
     */
  
  def formulaConvexity(comp:Compounding, freq:Frequency = Frequency.Annual):Option[Double] = discountCurve.flatMap { 
    case discount => 
      val cashflows = spotCashflowDayfrac(new Actual365Fixed)
      val discounter = (d:Double) => discount(d * 365.25)
      comp match {
        case Compounding.Simple => Duration.convexitySimple(cashflows, discounter)
        case Compounding.Continuous => Duration.convexityContinuous(cashflows, discounter)
        case Compounding.Compounded | Compounding.SimpleThenCompounded => Duration.convexityCompounded(cashflows, discounter, freq.toInteger, getYield(comp, freq).getOrElse(Double.NaN))
        case _ => None
      }
  }
    
  
}

