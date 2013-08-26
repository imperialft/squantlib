package squantlib.model.bond

import squantlib.pricing.model._
import squantlib.model.{Market, Bond, Underlying}
import squantlib.pricing.mcengine._
import squantlib.model.fx.FX
import squantlib.model.index.Index
import squantlib.model.equity.Equity

object DefaultBondSetting extends BondSetting {
  
  var productMapping:Map[String, BondSetting] = Map(
      
	  "DISC" -> NoModelSetting,
	  "SB" -> NoModelSetting,
	  "STEPUP" -> NoModelSetting,
	  
	  "FRN" -> ForwardModelSetting,
	  "FXFORWARD" -> ForwardModelSetting,
	  
	  "JGBR10F" -> JGBRModelSetting,
	  "JGBR10N" -> JGBRModelSetting,
	  "JGBR3" -> JGBRModelSetting,
	  "JGBR5" -> JGBRModelSetting,
	      
	  "JGBM10" -> JGBMModelSetting,
	  "JGBM5" -> JGBMModelSetting,
	  "JGBM2" -> JGBMModelSetting,

	  "CALLABLE" -> SwaptionModelSetting,
	  "SUC" -> SwaptionModelSetting,
	    
	  "DUAL" -> FXBulletModelSetting,
	    
	  "DCC" -> FXCallableModelSetting,
	  "PRDC" -> FXCallableModelSetting,
	  "FXLINKED" -> FXCallableModelSetting,
	  "RDC" -> FXCallableModelSetting,
	      
	  "NKY" -> IndexMcModelSetting,
	  "INDEX" -> IndexMcModelSetting,
	      
	  "EB" -> EquityMcModelSetting,
	  "ETF" -> EquityMcModelSetting,
	  
	  "EBW" -> NfMcModelSetting,
	  "INDEXW" -> NfMcModelSetting,
	  "FXW" -> NfMcModelSetting,
	  "ETFW" -> NfMcModelSetting,
	  "HYBRIDW" -> NfMcModelSetting,
	  
	  "EBQ" -> EquityQtoMcModelSetting,
	  "NKYQ" -> IndexQtoMcModelSetting,
	  "INDEXQ" -> IndexQtoMcModelSetting,
	  "ETFQ" -> EquityQtoMcModelSetting,
	  "FXQ" -> FxQtoMcModelSetting,
	  
	  "EBWQ" -> NfMcModelSetting,
	  "INDEXWQ" -> NfMcModelSetting,
	  "FXWQ" -> NfMcModelSetting,
	  "ETFWQ" -> NfMcModelSetting,
	  "HYBRIDWQ" -> NfMcModelSetting
	  
  )
  
  def apply(bond:Bond):Unit = productMapping.get(bond.db.productid) match {
    
    case None => {}
    
    case Some(f) => f(bond)
    
  }
    
}


object NoModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    bond.defaultModel = (m:Market, b:Bond) => NoModel(m, b)
	bond.forceModel = false
	bond.useCouponAsYield = false
    bond.requiresCalibration = false
  }
  
}

object ForwardModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    bond.defaultModel = (m:Market, b:Bond) => Forward(m, b)
	bond.forceModel = false
	bond.useCouponAsYield = false
    bond.requiresCalibration = false
  }
  
}

object JGBRModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    bond.defaultModel = (m:Market, b:Bond) => JGBRModel(m, b)
	bond.forceModel = true
	bond.useCouponAsYield = true
	bond.requiresCalibration = false
  }
  
}


object JGBMModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    bond.defaultModel = (m:Market, b:Bond) => JGBMModel(m, b)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}


object SwaptionModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    bond.defaultModel = (m:Market, b:Bond) => OneTimeSwaptionModel(m, b)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}


object FXBulletModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (fx:FX) => Bs1fContinuous(fx)
	bond.defaultModel = (m:Market, b:Bond) => FxMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}


object FXCallableModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (fx:FX) => Bs1fContinuous(fx)
	bond.defaultModel = (m:Market, b:Bond) => FxMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = true
  }
  
}


object IndexMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (index:Index) => Bs1fContinuous(index)
	bond.defaultModel = (m:Market, b:Bond) => IndexMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}

object EquityMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (equity:Equity) => Bs1fDiscrete(equity)
	bond.defaultModel = (m:Market, b:Bond) => EquityMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}

object NfMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (underlyings:List[Underlying]) => BsNf(underlyings)
	bond.defaultModel = (m:Market, b:Bond) => McNf(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
}

object IndexQtoMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (index:Index, fx:FX) => Bs1fQtoContinuous(index, fx)
	bond.defaultModel = (m:Market, b:Bond) => IndexQtoMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
}

object EquityQtoMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (equity:Equity, fx:FX) => Bs1fQtoDiscrete(equity, fx)
	bond.defaultModel = (m:Market, b:Bond) => EquityQtoMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
}

object FxQtoMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (fx:FX, qtofx:FX) => Bs1fQtoContinuous(fx, qtofx)
	bond.defaultModel = (m:Market, b:Bond) => FxQtoMc1f(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
}

object NfQtoMcModelSetting extends BondSetting {
  
  override def apply(bond:Bond) = {
    val engine = (underlyings:List[Underlying], fxs:List[Option[FX]]) => BsNfQto(underlyings, fxs)
	bond.defaultModel = (m:Market, b:Bond) => McNfQto(m, b, engine, 100000)
	bond.forceModel = true
	bond.useCouponAsYield = false
	bond.requiresCalibration = false
  }
  
}
