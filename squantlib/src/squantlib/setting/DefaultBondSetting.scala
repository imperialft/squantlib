package squantlib.setting

import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.pricing.model._
import scala.collection.mutable.Queue
import squantlib.model.{Market, Bond}
import squantlib.util.JsonUtils

object DefaultBondSetting {
  
	def apply(bond:Bond):Unit = bond.db.productid match {
	  
	  case "DISC" | "SB" | "STEPUP" => 
	    bond.modelSetter = (m:Market, b:Bond) => NoModel(m, b)
	    bond.forceModel = false
	    bond.useCouponAsYield = false
	    
	  case "JGBR10F" | "JGBR10N" | "JGBR3" | "JGBR5" => 
	    bond.modelSetter = (m:Market, b:Bond) => JGBRModel(m, b)
	    bond.forceModel = true
	    bond.useCouponAsYield = true

	  case "CALLABLE" | "SUC" => 
	    bond.modelSetter = (m:Market, b:Bond) => OneTimeSwaptionModel(m, b)
	    bond.forceModel = true
	    bond.useCouponAsYield = false
	    
//	    case "DUAL" => 
//	      try { 
//	        val engine = bond.settings.get("mcengine").asText
//	        Some((m:Market, b:Bond) => FXMontecarlo1f(m, b, engine))
//	      } catch {case _ => None}

	    case _ => None
	  }
  
}