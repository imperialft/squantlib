package squantlib.database

import squantlib.database.schemadefinitions.{ Bond => dbBond, CDSParameter, BondPrice, InputParameter}
import squantlib.model.discountcurve.{RateCurve, FXCurve, DiscountableCurve, CDSCurve, DiscountCurveFactory}
import squantlib.database.objectconstructor.{LiborDiscountCurveConstructor, FXDiscountCurveConstructor, CDSCurveConstructor, BondPriceConstructor}
import org.jquantlib.instruments.Bond
import org.jquantlib.termstructures.YieldTermStructure
import org.jquantlib.time.{Date => qlDate}

object QLConstructors {

	implicit def ToInputParameterSet(params:Traversable[InputParameter]) = new InputParameterSet(params)
  
	class InputParameterSet(val inputparameters:Traversable[InputParameter]){
	  def toLiborDiscountCurves:Iterable[RateCurve] = LiborDiscountCurveConstructor.getcurves(inputparameters)
	  def toFXDiscountCurves:Iterable[FXCurve] = FXDiscountCurveConstructor.getcurves(inputparameters)
	  def toDiscountCurves:Iterable[DiscountableCurve] = toLiborDiscountCurves ++ toFXDiscountCurves
	}
	
	implicit def ToCDSParameterSet(params:Traversable[CDSParameter]) = new CDSParameterSet(params)
	
	class CDSParameterSet(val cdsparameters:Traversable[CDSParameter]){
	  def toCDSCurves:Iterable[CDSCurve] = CDSCurveConstructor.getcurves(cdsparameters)
	}

	implicit def AddFuncs2Bond(bond:Bond) = new RichBond(bond)
	
	class RichBond(val bond:Bond){
	  def bondprice(valuedate:qlDate, factory:DiscountCurveFactory) = BondPriceConstructor.getprice(bond, factory)
	  def bondprice(valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure):BondPrice 
			= BondPriceConstructor.getprice(bond, valuedate, fx, paramset, termstructure)
	}
	
  
}