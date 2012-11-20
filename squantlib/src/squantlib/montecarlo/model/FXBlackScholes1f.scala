package squantlib.montecarlo.model

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import squantlib.montecarlo.randomgenerator
import scala.collection.SortedMap
import org.jquantlib.time.{Date => qlDate}
import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer
import squantlib.math.statistical.NormSInv
import squantlib.montecarlo.randomgenerator.MersenneTwister

class FXBlackScholes1f(var spot:Double, var ratedomF: Double => Double, var rateforF: Double => Double, var sigmaF: Double => Double) {

  var normSInv: Double => Double = (x:Double) => NormSInv(x)
  var randgen = new MersenneTwister(1)
  var rand: () => Double = () => randgen.sample
  
  def reset = randgen = new MersenneTwister(1)

  def generatePaths(eventDates:List[Double], paths:Int):Array[Array[Double]] = {
    
    val dates = eventDates.sorted
    val steps = dates.size
    
    val stepsize:List[Double] = dates.head :: (dates.tail, dates).zipped.map(_ - _)
    val ratedom = dates.map(ratedomF)
    val ratefor = dates.map(rateforF)
    val sigma = dates.map(sigmaF)
    
    def fratedom = ratedom.head :: (for (i <- (1 to steps-1).toList) yield ((ratedom(i) * dates(i) - ratedom(i-1) * dates(i-1)) / stepsize(i)))
    def fratefor = ratefor.head :: (for (i <- (1 to steps-1).toList) yield ((ratefor(i) * dates(i) - ratefor(i-1) * dates(i-1)) / stepsize(i)))
    def fvol = sigma.head :: (for (i <- (1 to steps-1).toList) yield ((dates(i) * sigma(i) * sigma(i) - dates(i-1) * sigma(i-1) * sigma(i-1)) / stepsize(i)))
        
	val drift = for (i <- 0 to steps-1) yield (fratedom(i) - fratefor(i) - ((fvol(i) * fvol(i)) / 2)) * stepsize(i)
	val sigt = for (i <- 0 to steps-1) yield fvol(i) * scala.math.sqrt(stepsize(i))

    for (path <- (0 to paths-1).toArray) yield {
      var spotprice = spot
      for (d <- (0 to steps-1).toArray) yield {
        val rnd = rand()
		val ninv1 = normSInv(rnd)
		spotprice *= scala.math.exp(drift(d) + (sigt(d) * ninv1))
		spotprice
      }
    }
  }  
  


}



  /* Encapsulates a payment leg information with bootstrapped forward vol and forward rate
   * @param eventdate underlying reference date in years
   * @param sigma	volatility of the underlying FX
   * @param ratedom	continuous risk-free rate of domestic pricing currency
   * @param ratefor	continuous risk-free rate of foreign currency
   * @param prev 	Previous cashflow (please create a dummy with zeros if first cashflow)
   */
class MCDate(val eventdate:Double, val sigma:Double, val ratedom:Double, val ratefor:Double, val prev:MCDate = null) {
  
	val (duration, fratedom, fratefor, fvol) = prev match {
	  case null => (eventdate, ratedom, ratefor, sigma)
	  case p => { 
	    val d = eventdate - p.eventdate
		(d, 
		(ratedom * eventdate - p.ratedom * p.eventdate) / d, 
		(ratefor * eventdate - p.ratefor * p.eventdate) / d, 
		math.sqrt((eventdate * sigma * sigma - p.eventdate * p.sigma * p.sigma) / d))
	}}
	
	val drift = (fratedom - fratefor - ((fvol * fvol) / 2)) * duration
	val sigt = fvol * scala.math.sqrt(duration)
	
	override def toString() = "event %.5s sigma %.5s rdom %.5s rfor %.5s t %.5s fdom %.5s ffor %.5s fvol %.5s drift %.7s sigt %.5s".format(
	    eventdate, sigma, ratedom, ratefor, duration, fratedom, fratefor,  fvol, drift, sigt)
	
}

//  /* Encapsulates monte-carlo results
//   * @param dates	calculation dates information
//   * @param modeloutput output from montecarlo engine (not discounted)
//   * @param legs 	number of legs
//   * @param result	results after discounting
//   * @param legprices price per payment leg
//   * @param price	montecarlo price
//   * @param pathprices price per calculation path
//   * @param stdev	standard deviation of the path results
//   */
//class MonteCarloResult(val dates:Array[CalculationPeriod1F], val modeloutput:Array[Array[Double]]) {
//    val legs = dates.size
//    val paths = modeloutput.size
//    val result:Array[Array[Double]] = modeloutput.map(m => (0 to legs-1).map(p => m(p) * dates(p).zc).toArray)
//    val legprices:Array[(CalculationPeriod1F, Double)] = (0 to legs-1).map(i => (dates(i), result.map(r => r(i)).sum / paths)).toArray
//    val pathprices:Array[Double] = result.map(_.sum)
//    val price:Double = legprices.map(_._2).sum
//    
//    val stdev = {
//      val pricer = new StandardDeviation
//      pricer.evaluate(pathprices) / math.sqrt(paths)
//    }
//    
//    override def toString() = dates.map(_.toString).mkString("\n") + 
//    					"\n#legs: " + legs + 
//    					"\n#paths: " + paths + 
//    					"\nprice: " + price + 
//    					"\nstdev : " + stdev + 
//    					"\nlegs: " + legprices.map(l => (l._1.eventdate + " => " + l._2)).mkString("\n")
//    
//}
//



//  
//  /* Simple montecarlo pricer for FX linked
//   * 1-factor on FX only (no rates volatility)
//   * volatility as function of time but constant & no smile
//   * @param spot 		current underlying price
//   * @param ratedom(t)	continuous risk-free rate of domestic pricing currency at time t as #years
//   * @param ratefor(t)	continuous risk-free rate of foreign currency at time t as #years
//   * @param sigma(t)	volatility of the underlying FX
//   * @param NormSInv 	Normal inverse cumulative distribution function
//   * @param Rand		random number generator
//   * @param Flow(t, f(x)) payoff function: for each time t, payment is f(x) where x is underlying price
//   * @param discount(t)	cash-flow discount rate ZC such that PV = amount x ZC
//   * @param path		number of paths
//   * @returns (resulting price, standard deviation)
//   */
//  def MCPrice(spot: Double, 
//      ratedom: Double => Double, 
//      ratefor: Double => Double, 
//      sigma: Double => Double, 
//      normSInv: Double => Double, 
//      rand:  () => Double, 
//      eventDates: Array[Double], 
//      payDates: Array[Double], 
//      flows: Array[Double => Double], 
//      discount: Double => Double, 
//      paths: Int):MonteCarloResult = {
//    
//    val inputdates = (0 to eventDates.size-1).map(i => 
//      new {val eventdate = eventDates(i); val paydate = payDates(i); val flow = flows(i)}).sortBy(d => d.eventdate)
//
//    var prev:CalculationPeriod1F = null
//    var datebuffer = ListBuffer.empty[CalculationPeriod1F]
//    (0 to inputdates.size - 1).foreach(i => {
//      val eventdate = inputdates(i).eventdate
//      val paydate = inputdates(i).paydate
//      val cp = new CalculationPeriod1F(
//	      eventdate = eventdate,
//	      paydate = paydate, 
//	      flow = inputdates(i).flow,
//	      sigma = sigma(eventdate),
//	      ratedom = ratedom(eventdate),
//	      ratefor = ratefor(eventdate),
//		  zc = discount(paydate),
//		  prev
//		)
//      prev = cp
//      datebuffer += cp 
//    })
//    
//    val dates = datebuffer.sortBy(_.eventdate).toArray
//    val nbdates = dates.size
//    val spotprice = Array.fill[Double](paths * 2)(spot)
//    val MonteCarloResult = Array.fill[Array[Double]](paths * 2)(new Array[Double](nbdates))
//
//    (0 to nbdates - 1).foreach {dateindex => {
//      val d = dates(dateindex)
//      (0 to paths - 1).foreach { path => {
//			val rnd = rand()
//			val ninv1 = normSInv(rnd)
//			spotprice(path*2) *= scala.math.exp(d.drift + (d.sigt * ninv1))
//			MonteCarloResult(path*2)(dateindex) = d.flow(spotprice(path*2))
//			
//			val ninv2 = -ninv1
//			spotprice(path*2+1) *= scala.math.exp(d.drift + (d.sigt * ninv2))
//			MonteCarloResult(path*2+1)(dateindex) = d.flow(spotprice(path*2+1))
//        }
//      }
//    }}
//    
//    new MonteCarloResult(dates, MonteCarloResult)
//    
//  }