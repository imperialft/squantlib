package net.squantlib.pricing.numerical

import net.squantlib.math.random.{RandomGenerator, MersenneTwister}
import net.squantlib.math.statistical.NormSInv
import org.apache.commons.math3.linear._

object BermudanSwaption {
  
  def getRandomGenerator:RandomGenerator = new MersenneTwister(1)
  
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
  
  class ExtendedMatrix(r:RealMatrix) {
    def apply(a:Int, b:Int) = get(a, b)
    def get(a:Int, b:Int) = r.getEntry(a, b)
    def set(a:Int, b:Int, v:Double) = r.setEntry(a, b, v)
    def setRow(row:Int, v:Double) = r.setRow(row, Array.fill(r.getColumnDimension)(v))
    def setColumn(col:Int, v:Double) = r.setColumn(col, Array.fill(r.getRowDimension)(v))
  }
  
}	