package squantlib.pricing

import squantlib.model.CurveFactory
import squantlib.payoff.Payoffs
import squantlib.pricing.model.BlackScholesFormula
import squantlib.model.fx.FX
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode

/* Pricing procedure:
 * 1) Get product information (and pricing settings if any)
 * 2) Initialize market with appropriate discounting
 * 3) Initialize pricing model and generate montecarlo paths(not necessary for fixed coupon)
 * 4) Initialize cashflow model
 * 5) Insert montecarlo paths into cashflow model and take average
 * 6) Discount
 */

class FXMontecarlo(val market:CurveFactory, val fx:FX, val payoffs:Payoffs, val issuer:String, val setting:JsonNode) {
	
//	def price(path:Int):Option[Double] = {
//	  
//	}
		
}

object FXMontecarlo {
  
//	def apply(market:CurveFactory, payoffs:Payoffs, issuer:String, setting:String):Option[FXMontecarlo] = {
//	  if (payoffs.variables.size != 1) { println("payoff not compatible with FX1d model"); return None}
//	  
//	  val fx = market.getFX(payoffs.variables.head)
//	  val params = setting.jsonNode
//	  val rateDom = (d:Double) => fx.rateDom(d.toDouble * 365.25)
//	  
//	  val model = (fx, params) match {
//	    case (Some(f), Some(p)) if p.get("model") == "FXBlackScholes1F" => 
//	  }
//	}
	  
}

