package squantlib.pricing.mcengine

import squantlib.math.random.{RandomGenerator, MersenneTwister}
import squantlib.math.statistical.NormSInv
import squantlib.model.equity.Equity
import squantlib.util.DisplayUtils._


/* Black-Scholes montecarlo generator with discrete dividends
 * - Discrete dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividends	dividend schedule described as date(#year from value date) => amount
 * @param repo(t)	repo rate at time t as number of years
*  * @param sigma(t)	volatility of the underlying index
 */

case class BlackScholesDiscreteDividends1f(
    var spot:Double, 
    var rate: Double => Double, 
    var dividends: Map[Double, Double], 
    var repoYield: Double => Double, 
    var volatility: Double => Double) 
    extends Montecarlo1f{
  
  var normSInv: Double => Double = (x:Double) => NormSInv(x)
  
  override var randomGenerator:RandomGenerator = new MersenneTwister(1)
  
  override def reset = randomGenerator = new MersenneTwister(1)

  /* Generates paths.
   * @param eventdates	observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted and duplicates removed by the function.
   * Check with first tuple argument for the order of dates.
  */
  
  override def generatePaths(eventDates:List[Double], paths:Int):(List[Double], List[List[Double]]) = {
    if (eventDates.isEmpty) {return (List.empty, List.empty)}
    
    reset
    
    val relavantDivs = dividends.filter(d => d._1 > 0.0 && d._1 <= eventDates.max)
    val eventWithDivs = eventDates.map(d => (d, 0.0)).toMap
    
    val eventDivs:List[(Double, Double)] = (eventWithDivs ++ relavantDivs).toList.sortBy(_._1)
    val dates = eventDivs.map(_._1)
    val divs = eventDivs.map(_._2)
    
    val sortedEventDates = eventDates.sorted
    val pathmapper = sortedEventDates.map(dates.indexOf(_))
        
    val steps = dates.size
    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)

    val ratedom = dates.map(rate)
    val ratefor = dates.map(d => repoYield(d))
    val sigma = dates.map(volatility)
    
    val fratedom = ratedom.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else (ratedom(i) * dates(i) - ratedom(i-1) * dates(i-1)) / stepsize(i)))
    
    val fratefor = ratefor.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else (ratefor(i) * dates(i) - ratefor(i-1) * dates(i-1)) / stepsize(i)))
    
    val fsigma = sigma.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else math.sqrt((dates(i) * sigma(i) * sigma(i) - dates(i-1) * sigma(i-1) * sigma(i-1)) / stepsize(i))))
    
	val drift = for (i <- 0 to steps-1) yield (fratedom(i) - fratefor(i) - ((fsigma(i) * fsigma(i)) / 2.0)) * stepsize(i)
	
	val sigt = for (i <- 0 to steps-1) yield fsigma(i) * scala.math.sqrt(stepsize(i))
	
    val genpaths = for (path <- (0 to paths-1).toList) yield {
      var spotprice = spot
      val apath = for (d <- (0 to steps-1).toList) yield {
        if (stepsize(d) == 0.0) spotprice
        else {
          val rnd = randomGenerator.sample
          val ninv1 = normSInv(rnd)
          spotprice = spotprice * scala.math.exp(drift(d) + (sigt(d) * ninv1)) - divs(d)
          spotprice
          }
      }
      pathmapper.map(apath)
    }
    
    (sortedEventDates, genpaths)
  }
  
  override def modelStatus = {
    var result = this.getClass.toString + "\n"
    val eventDates:List[Double] = (for(i <- 1 to 120 if (i <= 12 && i % 3 == 0)|| i % 12 == 0) yield i.toDouble / 12.0).toList
    
    val relavantDivs:Map[Double, Double] = dividends.filter(d => d._1 > 0.0 && d._1 <= eventDates.max).toMap
    val eventWithDivs:Map[Double, Double] = eventDates.map(d => (d, 0.0)).toMap
    
    val eventDivs:List[(Double, Double)] = (eventWithDivs ++ relavantDivs).toList.sortBy(_._1)
    val dates:List[Double] = eventDivs.map(_._1)
    
    val steps = dates.size
    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)

    val ratedom = dates.map(rate)
    val ratefor = dates.map(d => repoYield(d))
    val sigma = dates.map(volatility)
    
    val fratedom = ratedom.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else (ratedom(i) * dates(i) - ratedom(i-1) * dates(i-1)) / stepsize(i)))
    
    val fratefor = ratefor.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else (ratefor(i) * dates(i) - ratefor(i-1) * dates(i-1)) / stepsize(i)))
    
    val fsigma = sigma.head :: (for (i <- (1 to steps-1).toList) yield 
        (if (stepsize(i) == 0.0) 0.0 else math.sqrt((dates(i) * sigma(i) * sigma(i) - dates(i-1) * sigma(i-1) * sigma(i-1)) / stepsize(i))))
    
	val drift = for (i <- 0 to steps-1) yield (fratedom(i) - fratefor(i) - ((fsigma(i) * fsigma(i)) / 2.0)) * stepsize(i)
	
	result += "spot: " + spot + "\n"
	result += List("date", "frate", "repo", "sigma", "drift", "div").mkString("\t") + "\n"
	result += (0 to steps - 1).map(i => {
	  List(dates(i), fratedom(i), fratefor(i), fsigma(i), drift(i), relavantDivs.get(dates(i)).getOrElse(0.0)).map(_.asDouble).mkString("\t")
	}).mkString("\n")
	
    result
  }

}

object BlackScholesDiscreteDividends1f {
  
  def apply(equity:Equity):Option[BlackScholesDiscreteDividends1f] = 
	try { Some(new BlackScholesDiscreteDividends1f(equity.spot, equity.interestRateY, equity.dividendYears, equity.repoRateY, equity.volatilityY)) } 
	catch { case _ :Throwable=> None}
	
}

