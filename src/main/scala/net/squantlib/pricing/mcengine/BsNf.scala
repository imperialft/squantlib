package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister, ParkMiller}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.asset.Underlying
import net.squantlib.util.DisplayUtils._
import net.squantlib.math.statistical.{Mathematical, Cholesky}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)	continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

case class BsNf(
    variables:List[String],
    spot: List[Double], 
    rate: Double => Double, 
    dividendYield: List[Double => Double],
    repoYield: List[Double => Double], 
    dividends: List[Map[Double, Double]], 
    volatility: List[Double => Double],
    correl:Array[Array[Double]])
    extends MontecarloNf{
  
  override def getRandomGenerator:RandomGenerator = new MersenneTwister(1)

  /* Generates FX paths.
   * @param eventdates	FX observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted and duplicates removed by the function.
   * Check with first tuple argument for the order of dates.
  */
  
  def getEventDates(eventDates:List[Double]) = {
    val dividendDates = dividends.map(_.keySet.filter(d => d > 0.0 && d <= eventDates.max)).flatten.toSet
    (dividendDates ++ eventDates.toSet).toList.sorted
  }
  
  def getCholeskyMatrix:Array[Array[Double]] = {
    val decomp = Cholesky.decomposition(correl).orNull
    if (decomp == null) {errorOutput("Correlation matrix is not definite positive"); return Array.empty}
    val (chol, pChol) = decomp
    for (i <- 0 to chol.size - 1) chol(i)(i) = pChol(i)
    chol
  }
  
  def getForwards(
      mcdates:List[Double], 
      ratedom:List[Double], 
      ratefor:List[List[Double]], 
      sigma:List[List[Double]]):(List[Double], List[List[Double]], List[List[Double]]) = {
    
    val uls = spot.size
    
    @tailrec def acc[A](r:List[A], t:List[Double], f:(A, A, Double, Double) => A, d:A, current:List[A]):List[A] = 
      if (r.isEmpty || r.tail.isEmpty) current.reverse
      else acc(r.tail, t.tail, f, d, f(r.tail.head, r.head, t.tail.head, t.head) :: current)
      
    @tailrec def acc2(r0:List[Double], r1:List[Double], t0:Double, t1:Double, f:(Double, Double, Double, Double) => Double, current:List[Double]):List[Double] = 
      if (r0.isEmpty) current.reverse
      else acc2(r0.tail, r1.tail, t0, t1, f, f(r0.head, r1.head, t0, t1) :: current)

    val fratedom:List[Double] = ratedom.head :: acc[Double](ratedom, mcdates, (r0, r1, t0, t1) => (r1 * t1 - r0 * t0) / (t1 - t0), 0.0, List.empty)
    
    val fratefor:List[List[Double]] = ratefor.head :: acc[List[Double]](ratefor, mcdates, 
        (r0, r1, t0, t1) => acc2(r0, r1, t0, t1, (a0, a1, b0, b1) => (a1 * b1 - a0 * b0) / (b1 - b0), List.empty),
        List.fill(uls)(0.0), List.empty
    )
    
    val fsigma:List[List[Double]] = sigma.head :: acc[List[Double]](sigma, mcdates, 
        (r0, r1, t0, t1) => acc2(r0, r1, t0, t1, (a0, a1, b0, b1) => math.sqrt(math.max(0.00001, (a1 * a1 * b1 - a0 * a0 * b0) / (b1 - b0))), List.empty),
        List.fill(uls)(0.0), List.empty
    )
    
    (fratedom, fratefor, fsigma)
  }
  
  override def generatePaths[A](eventDates:List[Double], paths:Int, payoff:List[Map[String, Double]] => List[A]):(List[Double], List[List[A]]) = {
    if (eventDates.isEmpty) {return (List.empty, List.empty)}
    
    val chol = getCholeskyMatrix
    if (chol.isEmpty) {errorOutput("Correlation matrix is not definite positive"); return (List.empty, List.empty)}
    
    val mcdates:List[Double] = getEventDates(eventDates)
    val divs:List[List[Double]] = mcdates.map(d => (d, dividends.map(dd => dd.get(d).getOrElse(0.0)))).map(_._2)
    
    val stepsize = mcdates.head :: (mcdates.tail, mcdates).zipped.map(_ - _)
    val pathmapper = eventDates.sorted.map(mcdates.indexOf(_))
    
    val uls = spot.size
    val ratedom = mcdates.map(rate)
    val ratefor:List[List[Double]] = mcdates.map(d => (dividendYield, repoYield).zipped.map{case (dy, ry) => dy(d) + ry(d)})
    val sigma:List[List[Double]] = mcdates.map(d => volatility.map(_(d)))

    val (fratedom, fratefor, fsigma) = getForwards(mcdates, ratedom, ratefor, sigma)
    
	val drift:List[List[Double]] = driftacc(fratedom, fratefor, fsigma, stepsize, List.empty)
	
	val sigt:List[List[Double]] = (fsigma, stepsize).zipped.map{case (sig, ss) => sig.map(s => s * math.sqrt(ss))}
    
    @tailrec def iter(sp:List[Double], gauss:List[Double], dev:List[Double], drft:List[Double], divv:List[Double], current:List[Double]):List[Double] = 
      if (sp.isEmpty) current.reverse 
      else iter(sp.tail, gauss.tail, dev.tail, drft.tail, divv.tail, (sp.head * math.exp(gauss.head * dev.head + drft.head) - divv.head) :: current)
      
    val randomGenerator = getRandomGenerator
    def normSInv(x:Double) = NormSInv(x)
 
    @tailrec def getApath(steps:List[Double], drft:List[List[Double]], siggt:List[List[Double]], divv:List[List[Double]], current:List[List[Double]]):List[A] = 
      if (steps.isEmpty) {
        val pathmap = pathmapper.map(current.reverse.tail).map(l => (variables zip l).toMap)
        payoff(pathmap)
      }
      else {
        val independentGaussian = List.fill(uls)(normSInv(randomGenerator.sample))
        val correlatedGaussian = Mathematical.lowerTriangleMatrixMult(chol, independentGaussian)
        getApath(steps.tail, drft.tail, siggt.tail, divv.tail, iter(current.head, correlatedGaussian, siggt.head, drft.head, divv.head, List.empty) :: current)
      }
    
    @tailrec def getPathes(nbpath:Int, current:List[List[A]]):List[List[A]] = 
      if (nbpath == 0) current else getPathes(nbpath - 1, getApath(stepsize, drift, sigt, divs, List(spot))::current)
	
    (eventDates.sorted, getPathes(paths, List.empty))
  }
  
  @tailrec private def driftacc(rd:List[Double], rf:List[List[Double]], sig:List[List[Double]], stepp:List[Double], current:List[List[Double]]):List[List[Double]] = 
	if (rd.isEmpty) current.reverse
	else driftacc(rd.tail, rf.tail, sig.tail, stepp.tail, driftacc2(rd.head, rf.head, sig.head, stepp.head, List.empty) :: current)
    
  @tailrec private def driftacc2(rd:Double, rf:List[Double], sig:List[Double], stepp:Double, current:List[Double]):List[Double] = 
    if (rf.isEmpty) current.reverse
    else driftacc2(rd, rf.tail, sig.tail, stepp, (rd - rf.head - (sig.head * sig.head) / 2.0) * stepp :: current)
  
  override def modelName = this.getClass.toString
  
  override def spotref = spot
  
  override def scheduledDescription:(List[String], List[List[String]]) = {
    val basedates:List[Double] = (for(i <- 1 to 120 if i % 12 == 0) yield i.toDouble / 12.0).toList
    
    val chol = getCholeskyMatrix
    if (chol.isEmpty) {return (List("Correlation matrix is not definite positive"), List.empty)}
    
    val mcdates:List[Double] = getEventDates(basedates)
    val divs:List[List[Double]] = mcdates.map(d => (d, dividends.map(dd => dd.get(d).getOrElse(0.0)))).map(_._2)
    
    val stepsize = mcdates.head :: (mcdates.tail, mcdates).zipped.map(_ - _)
    val pathmapper = basedates.sorted.map(mcdates.indexOf(_))
    
    val uls = spot.size
    val ratedom = mcdates.map(rate)
    val ratefor:List[List[Double]] = mcdates.map(d => (dividendYield, repoYield).zipped.map{case (dy, ry) => dy(d) + ry(d)})
    val sigma:List[List[Double]] = mcdates.map(d => volatility.map(_(d)))

    val (fratedom, fratefor, fsigma) = getForwards(mcdates, ratedom, ratefor, sigma)
    
	val title = List("valuedate", "forward", "rate", "repo", "sigma", "div")
	
  	@tailrec def accr(adate:Double, aspots:List[Double], aratedom:Double, aratefors:List[Double], asiggs:List[Double], adivvs:List[Double], current:List[List[String]]):List[List[String]] = 
	    if (aspots.isEmpty) current.reverse
	    else {
	      val msg = List(adate.asDouble, aspots.head.asDouble, aratedom.asPercent(2), aratefors.head.asPercent(2), asiggs.head.asPercent(2), adivvs.head.asDouble)
	      accr(adate, aspots.tail, aratedom, aratefors.tail, asiggs.tail, adivvs.tail, msg :: current)
	}
    
	@tailrec def schedule(sp:List[Double], datez:List[Double], steps:List[Double], rd:List[Double], rf:List[List[Double]], sigg:List[List[Double]], divvs:List[List[Double]], acc:List[List[List[String]]]):List[List[List[String]]] = 
	    if (steps.isEmpty) acc.reverse
	    else {
	      val spp = (sp, rf.head, divvs.head).zipped.map{case (ssp, rrf, dvv) => ssp * scala.math.exp((rd.head - rrf) * steps.head) - dvv}
	      val msgs = accr(datez.head, spp, rd.head, rf.head, sigg.head, divvs.head, List.empty)
	      schedule(spp, datez.tail, steps.tail, rd.tail, rf.tail, sigg.tail, divvs.tail, msgs::acc)
	}
	  
	var spotprice = spot
	val aschedule = schedule(spot, mcdates, stepsize, fratedom, fratefor, fsigma, divs, List.empty)
	val s = aschedule.transpose.flatten
    
    val correlmatrix = correl.map(_.map(_.asPercent(2)).toList).toList
    val cholmatrix = chol.map(_.map(_.asPercent(2)).toList).toList

    (title, s ++ List(List("correlation")) ++ correlmatrix ++ List(List("cholesky")) ++ cholmatrix)
  }

}

object BsNf {
  
  def apply(uls:List[Underlying]):Option[BsNf] = 
	try {
	  val variables:List[String] = uls.map(_.id)
	  val spots:List[Double] = uls.map(_.spot)
	  val rates:Double => Double = uls.head.discountRateY
	  val dividendyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.assetYieldY; q})
	  val repoyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.repoRateY; q})

	  val dividends:List[Map[Double, Double]] = uls.map(_.dividendsY)
	  val volatility:List[Double => Double] = uls.map(ul => (d:Double) => ul.volatilityY(d))
	  val correl:Array[Array[Double]] = uls.map(ul => uls.map(u => u.impliedCorrelation(ul).getOrElse(Double.NaN)).toArray).toArray
	  
	  if (correl.exists(c => c.exists(d => d.isNaN))) None
	  else Some(new BsNf(variables, spots, rates, dividendyield, repoyield, dividends, volatility, correl)) 
	 } 
	catch { case _:Throwable => None}
	
}

