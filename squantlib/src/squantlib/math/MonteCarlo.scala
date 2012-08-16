package squantlib.math.montecarlo

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import squantlib.math.random._
import scala.collection.SortedMap
import org.jquantlib.time.{Date => qlDate}
import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer

object MonteCarlo_BS{

  /* Standard Black-Scholes calculation
   * @param spot 	current underlying price
   * @param ratedom	continuous risk-free rate of domestic pricing currency
   * @param ratefor	continuous risk-free rate of foreign currency
   * @param sigma	volatility of the underlying FX
   * @param NormSInv Normal inverse cumulative distribution function
   * @param time	time to maturity in years
   * @param discount cash-flow discount factor ZC such that PV = amount x ZC. None if it's discounted by ratedom.
   * @returns price
   */
  def blackScholes(spot:Double, ratedom:Double, ratefor:Double, sigma:Double, NormSInv:Double => Double, 
      strike:Double, time:Double, discount:Double = Double.NaN):Double = {

    val h1 = (math.log(spot / strike) + ((ratedom - ratefor) + sigma * sigma / 2) * time) / (sigma * math.sqrt(time))
    val h2 = h1 - sigma * math.sqrt(time)
    val price = (spot * math.exp(-ratefor * time) * NormSInv(h1) - strike * math.exp(-ratedom * time) * NormSInv(h2)) 
    if (discount.isNaN) price else price * math.exp(ratedom * time) * discount
  }
  
  /* Montecarlo price 1-factor, constant volatility, one payment cashflow.
   * @param spot 	current underlying price
   * @param ratedom	continuous risk-free rate of domestic pricing currency
   * @param ratefor	continuous risk-free rate of foreign currency
   * @param sigma	volatility of the underlying FX
   * @param NormSInv Normal inverse cumulative distribution function
   * @param Rand	random number generator
   * @param Flow	payoff function: given underlying price returns payoff.
   * 				for example, d:Double => Max(d - 100, 0) for basic European call.
   * @param time	time to maturity in years
   * @param discount cash-flow discount rate ZC such that PV = amount x ZC
   * @param path	number of paths
   * @returns (resulting price, standard deviation)
   */
  def simpleVanilla(spot:Double, 
      ratedom: Double, 
      ratefor: Double, 
      sigma: Double, 
      NormSInv: Double => Double, 
      Rand:()=> Double, 
      Flow: Double => Double, 
      time: Double, 
      discount: Double, 
      paths: Int):(Double, Double) = {
    
    val rndrift = (ratedom - ratefor - ((sigma * sigma) / 2)) * time
    val sigt = sigma * scala.math.sqrt(time)
    val stddev = new StandardDeviation
     
    val mcresult = (0 to paths - 1).map{i => {
        val rnd = Rand()
        val ninv = NormSInv(rnd)
        val underlying = spot * scala.math.exp(rndrift + (sigt * ninv))
        Flow(underlying)
     }}
    
    (mcresult.sum  / paths * discount, stddev.evaluate(mcresult.toArray) / math.sqrt(paths))
  }
  
  /* Simple montecarlo pricer for FX linked
   * 1-factor on FX only (no rates volatility)
   * volatility as function of time but constant & no smile
   * @param spot 		current underlying price
   * @param ratedom(t)	continuous risk-free rate of domestic pricing currency at time t as #years
   * @param ratefor(t)	continuous risk-free rate of foreign currency at time t as #years
   * @param sigma(t)	volatility of the underlying FX
   * @param NormSInv 	Normal inverse cumulative distribution function
   * @param Rand		random number generator
   * @param Flow(t, f(x)) payoff function: for each time t, payment is f(x) where x is underlying price
   * @param discount(t)	cash-flow discount rate ZC such that PV = amount x ZC
   * @param path		number of paths
   * @returns (resulting price, standard deviation)
   */
  def MCPrice(spot: Double, 
      ratedom: Double => Double, 
      ratefor: Double => Double, 
      sigma: Double => Double, 
      normSInv: Double => Double, 
      rand: () => Double, 
      eventDates: Array[Double], 
      payDates: Array[Double], 
      flows: Array[Double => Double], 
      discount: Double => Double, 
      paths: Int):MCResult = {
    
    val inputdates = (0 to eventDates.size-1).map(i => 
      new {val eventdate = eventDates(i); val paydate = payDates(i); val flow = flows(i)}).sortBy(d => d.eventdate)

    var prev:CalcPeriod = null
    var datebuffer = ListBuffer.empty[CalcPeriod]
    (0 to inputdates.size - 1).foreach(i => {
      val eventdate = inputdates(i).eventdate
      val paydate = inputdates(i).paydate
      val cp = new CalcPeriod(
	      eventdate = eventdate,
	      paydate = paydate, 
	      flow = inputdates(i).flow,
	      sigma = sigma(eventdate),
	      ratedom = ratedom(eventdate),
	      ratefor = ratefor(eventdate),
		  zc = discount(paydate),
		  prev
		)
      prev = cp
      datebuffer += cp 
    })
    
    val dates = datebuffer.sortBy(_.eventdate).toArray
    val nbdates = dates.size
    val spotprice = Array.fill[Double](paths)(spot)
    val mcresult = Array.fill[Array[Double]](paths)(new Array[Double](nbdates))

    (0 to nbdates - 1).foreach {dateindex => {
      val d = dates(dateindex)
      (0 to paths - 1).foreach { path => {
			val rnd = rand()
			val ninv = normSInv(rnd)
			spotprice(path) = spotprice(path) * scala.math.exp(d.drift + (d.sigt * ninv))
			mcresult(path)(dateindex) = d.flow(spotprice(path))
        }
      }
    }}
    
    new MCResult(dates, mcresult)
    
  }

  def MCPrice2(spot: Double, 
      ratedom: Double => Double, 
      ratefor: Double => Double, 
      sigma: Double => Double, 
      normSInv: Double => Double, 
      rand: () => Double, 
      eventDates: Array[Double], 
      payDates: Array[Double], 
      flows: Array[Double => Double], 
      discount: Double => Double, 
      paths: Int):MCResult = {
    
    val inputdates = (0 to eventDates.size-1).map(i => 
      new {val eventdate = eventDates(i); val paydate = payDates(i); val flow = flows(i)}).sortBy(d => d.eventdate)

    var prev:CalcPeriod = null
    var datebuffer = ListBuffer.empty[CalcPeriod]
    (0 to inputdates.size - 1).foreach(i => {
      val eventdate = inputdates(i).eventdate
      val paydate = inputdates(i).paydate
      val cp = new CalcPeriod(
	      eventdate = eventdate,
	      paydate = paydate, 
	      flow = inputdates(i).flow,
	      sigma = sigma(eventdate),
	      ratedom = ratedom(eventdate),
	      ratefor = ratefor(eventdate),
		  zc = discount(paydate),
		  prev
		)
      prev = cp
      datebuffer += cp 
    })
    
    val dates = datebuffer.sortBy(_.eventdate).toArray
    val nbdates = dates.size
    val spotprice = Array.fill[Double](paths * 2)(spot)
    val mcresult = Array.fill[Array[Double]](paths * 2)(new Array[Double](nbdates))

    (0 to nbdates - 1).foreach {dateindex => {
      val d = dates(dateindex)
      (0 to paths - 1).foreach { path => {
			val rnd = rand()
			val ninv1 = normSInv(rnd)
			spotprice(path*2) *= scala.math.exp(d.drift + (d.sigt * ninv1))
			mcresult(path*2)(dateindex) = d.flow(spotprice(path*2))
			
			val ninv2 = -ninv1
			spotprice(path*2+1) *= scala.math.exp(d.drift + (d.sigt * ninv2))
			mcresult(path*2+1)(dateindex) = d.flow(spotprice(path*2+1))
        }
      }
    }}
    
    new MCResult(dates, mcresult)
    
  }  
	def NormSInv(u:Double):Double =
	{
		// This function generates a standard normal random 
		// variable r from a uniform random variable in (0,1).
		// Note that u=0 or u=1 are not allowed.
	
		val (a0, a1, a2, a3) = (2.50662823884, -18.61500062529, 41.39119773534, -25.44106049637)
		val (b1, b2, b3, b4) = (-8.47351093090, 23.08336743743, -21.06224101826, 3.13082909833)
		val (c0, c1, c2) = (0.3374754822726147, 0.9761690190917186, 0.1607979714918209)
		val (c3, c4, c5) = (0.0276438810333863, 0.0038405729373609, 0.0003951896511919)
		val (c6, c7, c8) = (0.0000321767881768, 0.0000002888167364, 0.0000003960315187)
	
		val v = u - 0.5
		
		if (v > -0.42 && v < 0.42){
			val vv = v * v
			v * (((a3 * vv + a2) * vv + a1) * vv + a0)/((((b4 * vv + b3) * vv + b2) * vv + b1) * vv + 1.0)
		}
		else {
			val w = if (v > 0) math.log(-math.log(1-u)) else math.log(-math.log(u));
			val r=(((((((c8*w+c7)*w+c6)*w+c5)*w+c4)*w+c3)*w+c2)*w+c1)*w+c0;
			if (v < 0) -r else r
		}
	}	
}



  /* Encapsulates a payment leg information with bootstrapped forward vol and forward rate
   * @param eventdate underlying reference date in years
   * @param paydate payment date in years
   * @param daycount daycount fraction eg ~0.5 for semiannual coupon, etc
   * @param flow	cashflow function given underlying price
   * @param sigma	volatility of the underlying FX
   * @param ratedom	continuous risk-free rate of domestic pricing currency
   * @param ratefor	continuous risk-free rate of foreign currency
   * @param zc		Zero-coupon rate to discount the resulting cashflow
   * @param prev 	Previous cashflow (please create a dummy with zeros if first cashflow)
   * @returns price
   */
class CalcPeriod(var eventdate:Double, var paydate:Double, var flow:Double => Double,  
    var sigma:Double, var ratedom:Double, var ratefor:Double, var zc:Double, val prev:CalcPeriod = null) {
  
    val duration = if (prev == null) eventdate else eventdate - prev.eventdate
	val fratedom = if (prev == null) ratedom else (ratedom * eventdate - prev.ratedom * prev.eventdate) / duration
	val fratefor = if (prev == null) ratefor else (ratefor * eventdate - prev.ratefor * prev.eventdate) / duration
	val fvol = if (prev == null) sigma else math.sqrt((eventdate * sigma * sigma - prev.eventdate * prev.sigma * prev.sigma) / duration)
	val drift = (fratedom - fratefor - ((fvol * fvol) / 2)) * duration
	val sigt = fvol * scala.math.sqrt(duration)
	
	override def toString() = "event %.5s pay %.5s sigma %.5s rdom %.5s rfor %.5s zc %.5s t %.5s fdom %.5s ffor %.5s fvol %.5s drift %.7s sigt %.5s".format(
	    eventdate, paydate, sigma, ratedom, ratefor, zc, duration, fratedom, fratefor,  fvol, drift, sigt)
	
}

  /* Encapsulates monte-carlo results
   * @param dates	calculation dates information
   * @param modeloutput output from montecarlo engine (not discounted)
   * @param legs 	number of legs
   * @param result	results after discounting
   * @param legprices price per payment leg
   * @param price	montecarlo price
   * @param pathprices price per calculation path
   * @param stdev	standard deviation of the path results
   */
class MCResult(val dates:Array[CalcPeriod], val modeloutput:Array[Array[Double]]) {
    val legs = dates.size
    val paths = modeloutput.size
    val result:Array[Array[Double]] = modeloutput.map(m => (0 to legs-1).map(p => m(p) * dates(p).zc).toArray)
    val legprices:Array[(CalcPeriod, Double)] = (0 to legs-1).map(i => (dates(i), result.map(r => r(i)).sum / paths)).toArray
    val pathprices:Array[Double] = result.map(_.sum)
    val price:Double = legprices.map(_._2).sum
    
    val stdev = {
      val pricer = new StandardDeviation
      pricer.evaluate(pathprices) / math.sqrt(paths)
    }
    
    override def toString() = dates.map(_.toString).mkString("\n") + 
    					"\n#legs: " + legs + 
    					"\n#paths: " + paths + 
    					"\nprice: " + price + 
    					"\nstdev : " + stdev + 
    					"\nlegs: " + legprices.map(l => (l._1.eventdate + " => " + l._2)).mkString("\n")
    
}
