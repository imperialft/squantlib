package squantlib.setting

import squantlib.pricing.model._
import squantlib.model.{Market, Bond}
import squantlib.pricing.mcengine._
import squantlib.model.fx.FX
import squantlib.model.index.Index

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
	      
	  case "JGBM10" | "JGBM5" | "JGBM2" => 
	      bond.defaultModel = (m:Market, b:Bond) => JGBMModel(m, b)
	      bond.forceModel = true
	      bond.useCouponAsYield = false
	      bond.requiresCalibration = false

	  case "CALLABLE" | "SUC" => 
	      bond.defaultModel = (m:Market, b:Bond) => OneTimeSwaptionModel(m, b)
	      bond.forceModel = true
	      bond.useCouponAsYield = false
	      bond.requiresCalibration = false
	    
	  case "DUAL" => 
	      val engine = (fx:FX) => BlackScholes1f(fx)
	      bond.defaultModel = (m:Market, b:Bond) => FXMontecarlo1f(m, b, engine, 100000)
	      bond.forceModel = true
	      bond.useCouponAsYield = false
	      bond.requiresCalibration = false
	    
	  case "DCC" | "PRDC" => 
	      val engine = (fx:FX) => BlackScholes1f(fx)
	      bond.defaultModel = (m:Market, b:Bond) => FXMontecarlo1f(m, b, engine, 100000)
	      bond.forceModel = true
	      bond.useCouponAsYield = false
	      bond.requiresCalibration = true
	      
	  case "NKY" => 
	      val engine = (index:Index) => BlackScholesWithRepo(index)
	      bond.defaultModel = (m:Market, b:Bond) => IndexMontecarlo1f(m, b, engine, 30000)
	      bond.forceModel = true
	      bond.useCouponAsYield = false
	      bond.requiresCalibration = false
	    
	  case _ => None
	}
}