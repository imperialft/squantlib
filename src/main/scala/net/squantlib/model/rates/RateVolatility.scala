package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.util.Date

/**
 * Encapsulates rate volatility surface, as function of expiry, maturity and strike.
 */

case class RateVolatility(valuedate:Date, surface:(Double, Double, Option[Double]) => Double) {
  
  def value(expiry:Double, maturity:Double) = surface(expiry, maturity, None)
  def apply(expiry:Double, maturity:Double) = value(expiry, maturity)
  
  def value(expiry:Double, maturity:Double, strike:Double) = surface(expiry, maturity, Some(strike))
  def apply(expiry:Double, maturity:Double, strike:Double) = value(expiry, maturity, strike)
  
  def value(expiry:Date, maturity:Date) = surface(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - valuedate.serialNumber, None)
  def apply(expiry:Date, maturity:Date) = value(expiry, maturity)
  
  def value(expiry:Date, maturity:Date, strike:Double) = surface(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - valuedate.serialNumber, Some(strike))
  def apply(expiry:Date, maturity:Date, strike:Double) = value(expiry, maturity, strike)
  
  def value(expiry:qlPeriod, maturity:qlPeriod) = surface(valuedate.days(expiry), valuedate.days(maturity), None)
  def apply(expiry:qlPeriod, maturity:qlPeriod) = value(expiry, maturity)
  
  def value(expiry:qlPeriod, maturity:qlPeriod, strike:Double) = surface(valuedate.days(expiry), valuedate.days(maturity), Some(strike))
  def apply(expiry:qlPeriod, maturity:qlPeriod, strike:Double) = value(expiry, maturity, strike)
}

object RateVolatility {
  
  def apply(vd:Date, flatValue:Double):RateVolatility 
  = RateVolatility(vd, (exp:Double, mat:Double, stk:Option[Double]) => flatValue)
  
  def apply(surface:YieldParameter3D):RateVolatility 
  = RateVolatility(surface.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => surface(exp, mat))
  
  def apply(valuedate:Date, points:Map[(qlPeriod, qlPeriod), Double]):RateVolatility 
  = apply(YieldParameter3D(valuedate, points))
  
  def expiryFunction(vector:YieldParameter):RateVolatility 
  = RateVolatility(vector.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => vector(exp))
  
  def maturityFunction(vector:YieldParameter):RateVolatility 
  = RateVolatility(vector.valuedate, (exp:Double, mat:Double, stk:Option[Double]) => vector(mat))
  
}