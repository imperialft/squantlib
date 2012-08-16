
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */

import org.apache.commons.math3.distribution.NormalDistribution
import squantlib.math.random._
import squantlib.math.montecarlo.MonteCarlo_BS


val spot = 100.0
val ratedom = (d:Double) => 0.02
val ratefor = (d:Double) => 0.08
val sigma = (d:Double) => 0.30
val normdist = new NormalDistribution
//val NormSInv = (d:Double) => normdist.inverseCumulativeProbability(d)
val NormSInv = (d:Double) => MonteCarlo_BS.NormSInv(d)
val NormSDist = (d:Double) => normdist.cumulativeProbability(d)
val eventdates = Array(5.0)
val paydates = Array(5.0)
val strike = 0.0
val flow = Array.fill(1)((d:Double) => if (d > strike) d - strike else 0.0)
val discount = (d:Double) => 1.0
val paths = 500000

println("Mersenne Twister")
var t0 = System.nanoTime
val generator_mt = new MersenneTwister(1)
val rand_mt = () => generator_mt.sample
val mcresult1 = MonteCarlo_BS.MCPrice(spot, ratedom, ratefor, sigma, NormSInv, rand_mt, eventdates, paydates, flow, discount, paths)
println("result: \n" + mcresult1)
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

println("Mersenne Twister with inverse")
t0 = System.nanoTime
val generator_mt2 = new MersenneTwister(1)
val rand_mt2 = () => generator_mt2.sample
val mcresult2 = MonteCarlo_BS.MCPrice2(spot, ratedom, ratefor, sigma, NormSInv, rand_mt2, eventdates, paydates, flow, discount, paths/2)
println("result: \n" + mcresult2)
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

println("Check Black Scholes:")
val pricebs = (0 to eventdates.size-1).map{ i => {
  val event = eventdates(i)
  val price = MonteCarlo_BS.blackScholes(spot, ratedom(event), ratefor(event), sigma(event), NormSDist, strike, event, discount(event))
  println("i = " + i + " => bs price = " + price)
}}

val yr = eventdates(0)
val vol = sigma(yr)
val fwd = spot * math.exp(-ratefor(yr) * yr) / math.exp(-ratedom(yr) * yr)
val sigma1 = mcresult2.pathprices.filter(p => p >= fwd * (1 - vol*math.sqrt(yr)) && p <= fwd * (1 + vol*math.sqrt(yr))).size
println("sigma1.0 " + sigma1 + " / " + paths + " => " + sigma1.toDouble / paths)

val sigma2 = mcresult2.pathprices.filter(p => p >= fwd * (1 - vol*2*math.sqrt(yr)) && p <= fwd * (1 + vol*2*math.sqrt(yr))).size
println("sigma2.0 " + sigma2 + " / " + paths + " => " + sigma2.toDouble / paths)

val sigma233 = mcresult2.pathprices.filter(p => p >= fwd * (1 - vol*2.33*math.sqrt(yr)) && p <= fwd * (1 + vol*2.33*math.sqrt(yr))).size
println("sigma2.33 " + sigma233 + " / " + paths + " => " + sigma233.toDouble / paths)

val sigma3 = mcresult2.pathprices.filter(p => p >= fwd * (1 - vol*3*math.sqrt(yr)) && p <= fwd * (1 + vol*3*math.sqrt(yr))).size
println("sigma3 " + sigma3 + " / " + paths + " => " + sigma3.toDouble / paths)
