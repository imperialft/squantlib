package squantlib.setting

import squantlib.pricing.model._
import squantlib.model.{Market, Bond}

object DefaultBondSetting {
  
	def apply(bond:Bond):Unit = bond.db.productid match {
	  
		  case "DISC" | "SB" | "STEPUP" =>
		      bond.defaultModel = (m:Market, b:Bond) => NoModel(m, b)
			  bond.forceModel = false
			  bond.useCouponAsYield = false
		      bond.requiresCalibration = false
		    
		  case "JGBR10F" | "JGBR10N" | "JGBR3" | "JGBR5" => 
		      bond.defaultModel = (m:Market, b:Bond) => JGBRModel(m, b)
		      bond.forceModel = true
		      bond.useCouponAsYield = true
		      bond.requiresCalibration = false
	
		  case "CALLABLE" | "SUC" => 
		      bond.defaultModel = (m:Market, b:Bond) => OneTimeSwaptionModel(m, b)
		      bond.forceModel = true
		      bond.useCouponAsYield = false
		      bond.requiresCalibration = false
		    
		  case "DUAL" => 
		      val engine = if (bond.settings has "mcengine") bond.settings.get("mcengine").asText else "FXBlackScholes1f"
		      bond.defaultModel = (m:Market, b:Bond) => FXMontecarlo1f(m, b, engine)
		      bond.forceModel = true
		      bond.useCouponAsYield = false
		      bond.requiresCalibration = false
		    
		  case "DCC" | "PRDC" => 
		      val engine = if (bond.settings has "mcengine") bond.settings.get("mcengine").asText else "FXBlackScholes1f"
		      bond.defaultModel = (m:Market, b:Bond) => FXMontecarlo1f(m, b, engine)
		      bond.forceModel = true
		      bond.useCouponAsYield = false
		      bond.requiresCalibration = true
		    
		  case _ => None
	}
}