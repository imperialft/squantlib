package squantlib.montecarlo.valuation

object BlackScholesFormula {
  
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
  
}