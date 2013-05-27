package squantlib.model.bond

import squantlib.model.Bond

trait BondSetting {
  
  def apply(bond:Bond):Unit
  
}

object BondSetting {
  
  def getDefault:BondSetting = DefaultBondSetting
  
}