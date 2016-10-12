package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.fx.FX
import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec
import net.squantlib.model.index.Index
import scala.collection.mutable.ListBuffer

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)	continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

class Bs1fContinuous(
    spot:Double, 
    rate: Double => Double, 
    dividendYield: Double => Double, 
    volatility: Double => Double) 
    extends Montecarlo1f{
  
  override def getRandomGenerator:RandomGenerator = new MersenneTwister(1)

  val smallvalue = 0.00001
  
  /* Generates FX paths.
   * @param eventdates	FX observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted and duplicates removed by the function.
   * Check with first tuple argument for the order of dates.
  */
  
  override def generatePaths(eventDates:List[Double], paths:Int, payoff:List[Double] => List[Double]):(List[Double], List[List[Double]]) = {
    if (eventDates.isEmpty) {return (List.empty, List.empty)}
    
    val randomGenerator = getRandomGenerator
    def normSInv(x:Double) = NormSInv(x)
    
    val dates = eventDates.sorted
    val steps = dates.size
    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)

    val ratedom = dates.map(rate)
    val ratefor = dates.map(dividendYield)
    val sigma = dates.map(volatility)

    val fratedom:List[Double] = ratedom.head :: acc[Double](ratedom, dates, (r0, r1, t0, t1) => (r1 * t1 - r0 * t0) / (t1 - t0), 0.0, List.empty)
    
    val fratefor:List[Double] = ratefor.head :: acc[Double](ratefor, dates, (r0, r1, t0, t1) => (r1 * t1 - r0 * t0) / (t1 - t0), 0.0, List.empty)
    
    val fsigma:List[Double] = sigma.head :: acc[Double](sigma, dates, (a0, a1, b0, b1) => math.sqrt(math.max(0.000001, (a1 * a1 * b1 - a0 * a0 * b0) / (b1 - b0))), 0.0, List.empty)
    
    val drift:List[Double] = driftacc(fratedom, fratefor, fsigma, stepsize, List.empty)
	
    val sigt:List[Double] = (fsigma, stepsize).zipped.map{case (sig, ss) => sig * math.sqrt(ss)}
    
    @tailrec def getApath(steps:List[Double], drft:List[Double], siggt:List[Double], current:List[Double]):List[Double] = {
      if (steps.isEmpty) payoff(current.reverse.tail)
      else {
        val rnd = randomGenerator.sample
        val ninv1 = normSInv(rnd)
        val spot = if (steps.head < smallvalue) current.head else current.head * scala.math.exp(drft.head + (siggt.head * ninv1))
        getApath(steps.tail, drft.tail, siggt.tail, spot :: current)
      }
    }
    
    val result:ListBuffer[List[Double]] = ListBuffer.empty
    Range(0, paths).map(i => result.append(getApath(stepsize, drift, sigt, List(spot))))
    (dates, result.toList)
  }

  @tailrec private def acc[A](r:List[A], t:List[Double], f:(A, A, Double, Double) => A, d:A, current:List[A]):List[A] = 
    if (r.isEmpty || r.tail.isEmpty) current.reverse
    else if (t.tail.head - t.head < smallvalue)  acc(r.tail, t.tail, f, d, d :: current)
    else acc(r.tail, t.tail, f, d, f(r.tail.head, r.head, t.tail.head, t.head) :: current)
    
  @tailrec private def driftacc(rd:List[Double], rf:List[Double], sig:List[Double], stepp:List[Double], current:List[Double]):List[Double] = 
    if (rd.isEmpty) current.reverse
    else driftacc(rd.tail, rf.tail, sig.tail, stepp.tail, (rd.head - rf.head - ((sig.head * sig.head) / 2.0)) * stepp.head :: current)
	
  override def modelName = this.getClass.toString
  
  override def spotref = List(spot)
  
  override def scheduledDescription = {
    val dates:List[Double] = (for(i <- 1 to 120 if (i <= 12 && i % 3 == 0)|| i % 12 == 0) yield i.toDouble / 12.0).toList ++ List(12.0, 15.0, 20.0)
    
    val steps = dates.size
    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)

    val ratedom = dates.map(rate)
    val ratefor = dates.map(dividendYield)
    val sigma = dates.map(volatility)

    val fratedom:List[Double] = ratedom.head :: acc[Double](ratedom, dates, (r0, r1, t0, t1) => (r1 * t1 - r0 * t0) / (t1 - t0), 0.0, List.empty)
    
    val fratefor:List[Double] = ratefor.head :: acc[Double](ratefor, dates, (r0, r1, t0, t1) => (r1 * t1 - r0 * t0) / (t1 - t0), 0.0, List.empty)
    
    val fsigma:List[Double] = sigma.head :: acc[Double](sigma, dates, (a0, a1, b0, b1) => math.sqrt(math.max(0.000001, (a1 * a1 * b1 - a0 * a0 * b0) / (b1 - b0))), 0.0, List.empty)
    
    val drift:List[Double] = driftacc(fratedom, fratefor, fsigma, stepsize, List.empty)
    	
    val sigt:List[Double] = (fsigma, stepsize).zipped.map{case (sig, ss) => sig * math.sqrt(ss)}
    	
    var spotprice = spot
    val title = List("valuedate", "forward", "rate ccy1", "rate ccy2", "sigma", "drift")
    val schedule = (0 to steps - 1).toList.map(i => {
	  spotprice *= scala.math.exp((fratedom(i) - fratefor(i)) * stepsize(i))
	  List(dates(i).asDouble, spotprice.asDouble, fratedom(i).asPercent(2), fratefor(i).asPercent(2), fsigma(i).asPercent(2), drift(i).asDouble)
	})
	
    (title, schedule)
  }

}

object Bs1fContinuous {
  
  def apply(fx:FX):Option[Bs1fContinuous] = 
    try { Some(new Bs1fContinuous(fx.spot, fx.rateDomY, fx.rateForY, fx.volatilityY)) } 
    catch { case _:Throwable => None}
	
  def apply(index:Index):Option[Bs1fContinuous] = 
    try { Some(new Bs1fContinuous(index.spot, index.discountRateY, d => index.dividendYieldY(d) + index.repoRateY(d), index.volatilityY)) } 
    catch { case _:Throwable => None}
	
}

