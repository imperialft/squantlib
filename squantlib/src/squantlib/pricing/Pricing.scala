package squantlib.pricing

import squantlib.model.CurveFactory
import squantlib.payoff.{Payoffs, Schedule}
import squantlib.pricing.model._
import squantlib.model.fx.FX
import squantlib.model.Bond
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

class FXMontecarlo1f( val market:CurveFactory, 
					  val model:Montecarlo1f, 
					  val payoffs:Payoffs, 
					  val schedule:Schedule,
					  val issuer:String, 
					  val setting:JsonNode) {
  
	val eventDates = schedule.eventDates
	
  
	def price(paths:Int):Double = {
	  0.0
	  
	}
		
}

object FXMontecarlo1f {
  
	def apply(market:CurveFactory, bond:Bond, issuer:String, setting:String):Option[FXMontecarlo1f] = {
	  
	  if (!bond.isValidCoupon) { println("bond schedule is invalid"); return None}
	  
	  val payoffs = bond.coupon
	  val schedule = bond.schedule
	  
	  if (payoffs.variables.size != 1) { println("payoff not compatible with FX1d model"); return None}
	  
	  val fx = market.getFX(payoffs.variables.head)
	  val fxsetting = setting.jsonNode
	  
	  val mcmodel:Option[Montecarlo1f] = fx match {
	    case None => None
	    case Some(f) => setting.parseJsonString("model") match {
	      	case "FXBlackScholes1F"  => FXBlackScholes1f(f)
	        case "FXzeroVol" => FXzeroVol1f(f)
	        case _ => None
	    }
	  }
	  
	  (fxsetting, mcmodel, schedule) match {
	    case (Some(s), Some(m), Some(sc)) => Some(new FXMontecarlo1f(market, m, payoffs, sc, issuer, s))
	    case _ => None
	  }
	}
}

