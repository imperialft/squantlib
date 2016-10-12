package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import net.squantlib.util.initializer.Currencies
import org.jquantlib.currencies.Currency


/**
 * @constructor stores each information 
 * @param floatindex can take any maturity.
 */
case class CDSCurve(val rate:YieldParameter, val currency:Currency, val issuerid:String) extends AbstractCurve{
  def this(r:YieldParameter, c:String, id:String) = this(r, Currencies.getOrElse(c, null), id)
  
  override def shifted(v:(Double, Double) => Double):CDSCurve = new CDSCurve(rate.shifted(v), currency, issuerid)
}

