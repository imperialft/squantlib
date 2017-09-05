package net.squantlib.math.volatility

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

object OptionUtilities {

  
  def strikeToDelta(
    strike:Double,
    forward:Double,
    expiry:Double,
    atmVolatility:Double,
    rate:Double,
    dividend:Double
    ):Double = {

    val d1 = (math.log(forward / strike) + (rate - divident + atmVolatility * atmvVolatility) * expiry) / (atmVolatility * expiry)
    Double.NaN

  }
  

  
}

