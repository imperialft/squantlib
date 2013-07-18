package squantlib.model.rates

import squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Encapsulates rate volatility surface, as function of expiry, maturity and strike.
 */

case class RateVolatility(valuedate:qlDate, surface:(Double, Double, Option[Double]) => Double) {
  
  def value(expiry:Double, maturity:Double) = surface(expiry, maturity, None)
  def apply(expiry:Double, maturity:Double) = value(expiry, maturity)
  
  def value(expiry:Double, maturity:Double, strike:Double) = surface(expiry, maturity, Some(strike))
  def apply(expiry:Double, maturity:Double, strike:Double) = value(expiry, maturity, strike)
  
  def value(expiry:qlDate, maturity:qlDate) = surface(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - valuedate.serialNumber, None)
  def apply(expiry:qlDate, maturity:qlDate) = value(expiry, maturity)
  
  def value(expiry:qlDate, maturity:qlDate, strike:Double) = surface(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - valuedate.serialNumber, Some(strike))
  def apply(expiry:qlDate, maturity:qlDate, strike:Double) = value(expiry, maturity, strike)
  
  def value(expiry:qlPeriod, maturity:qlPeriod) = surface(expiry.days(valuedate), maturity.days(valuedate), None)
  def apply(expiry:qlPeriod, maturity:qlPeriod) = value(expiry, maturity)
  
  def value(expiry:qlPeriod, maturity:qlPeriod, strike:Double) = surface(expiry.days(valuedate), maturity.days(valuedate), Some(strike))
  def apply(expiry:qlPeriod, maturity:qlPeriod, strike:Double) = value(expiry, maturity, strike)
}

object RateVolatility {
  
  def apply(vd:qlDate, flatValue:Double):RateVolatility 
  = RateVolatility(vd, (exp:Double, mat:Double, stk:Option[Double]) => flatValue)
  
  def apply(surface:YieldParameter3D):RateVolatility 
  = RateVolatility(surface.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => surface(exp, mat))
  
  def apply(valuedate:qlDate, points:Map[(qlPeriod, qlPeriod), Double]):RateVolatility 
  = apply(YieldParameter3D(valuedate, points))
  
  def expiryFunction(vector:YieldParameter):RateVolatility 
  = RateVolatility(vector.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => vector(exp))
  
  def maturityFunction(vector:YieldParameter):RateVolatility 
  = RateVolatility(vector.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => vector(mat))
  
}