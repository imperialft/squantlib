package squantlib.database

import squantlib.database.schemadefinitions.{ Bond => dbBond, CDSParameter, BondPrice, RateFXParameter}
import squantlib.model.discountcurve.{RateCurve, FXCurve, DiscountableCurve, CDSCurve, DiscountCurveFactory}
import squantlib.database.objectconstructor.{LiborDiscountCurveConstructor, FXDiscountCurveConstructor, CDSCurveConstructor, BondPriceConstructor}
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.termstructures.YieldTermStructure
import org.jquantlib.time.{Date => qlDate, TimeSeries}
import scala.collection.SortedMap
import java.lang.{Double => JavaDouble}
import java.util.{Date => JavaDate}
import scala.collection.JavaConversions._

/**
* Implicit functions to convert between database objects and model objects
*/
object QLConstructors {
  
	implicit def JavaDate2QlDate(d:JavaDate) = new qlDate(d)
	implicit def QlDate2JavaDate(d:qlDate) = d.longDate

	implicit def RateFXParameter2ParamSet(params:Traversable[RateFXParameter]) = new RateFXParameterSet(params)
 	class RateFXParameterSet(val parameters:Traversable[RateFXParameter]){
	  def toLiborDiscountCurves:Iterable[RateCurve] = LiborDiscountCurveConstructor.getcurves(parameters)
	  def toFXDiscountCurves:Iterable[FXCurve] = FXDiscountCurveConstructor.getcurves(parameters)
	  def toDiscountCurves:Iterable[DiscountableCurve] = toLiborDiscountCurves ++ toFXDiscountCurves
	}

	
	implicit def CDSParameter2ParamSet(params:Traversable[CDSParameter]) = new CDSParameterSet(params)
	class CDSParameterSet(val cdsparameters:Traversable[CDSParameter]){
	  def toCDSCurves:Iterable[CDSCurve] = CDSCurveConstructor.getcurves(cdsparameters)
	}


//	implicit def FXRate2FXRateSet(params:Traversable[FXRate]) = new FXRateSet(params)
//	class FXRateSet(val fxset:Traversable[FXRate]){
//	  def fxjpy(ccy:String):Double = try { fxset.filter(fx => fx.currencyid == ccy).head.fxjpy }
//	  								catch {case e => Double.NaN}
//	   
//	  def fx(ccy1:String, ccy2:String):Double = if (ccy1 == ccy2) 1.0
//			  							else if (ccy1 == "JPY") 1.0/fxjpy(ccy2)
//	  									else if (ccy2 == "JPY") fxjpy(ccy1)
//	  									else fxjpy(ccy1) / fxjpy(ccy2)
//	  									
//	  def fxusd(ccy:String):Double = fx("USD", ccy)
//	  
//	  def toInputParameter:Set[InputParameter] = fxset.map(fx => 
//	    new InputParameter(
//	      id = -fx.id,
//	      paramset = fx.paramset, 
//	      paramdate = fx.paramdate, 
//	      instrument = "FX", 
//	      asset = fx.currencyid, 
//	      maturity = null, 
//	      value = fxusd(fx.currencyid), 
//	      option = null, 
//	      comment = null, 
//	      created = fx.lastmodified
//	      )).toSet + inputParamJpy
//	      
//	  def inputParamJpy:InputParameter = 
//	    new InputParameter(
//	      id = -(fxset.map(_.id).max + 1),
//	      paramset = fxset.head.paramset, 
//	      paramdate = fxset.head.paramdate, 
//	      instrument = "FX", 
//	      asset = "JPY", 
//	      maturity = null, 
//	      value = fxusd("JPY"), 
//	      option = null, 
//	      comment = null, 
//	      created = fxset.head.lastmodified
//	      )
//	}

	
//    implicit def SortedMap2Ts(values:SortedMap[qlDate, Double]) : TimeSeries[JavaDouble] = 
//      new TimeSeries[java.lang.Double](JavaDouble.TYPE, mapAsJavaMap(values.mapValues(q => q.doubleValue)))
//        
//    implicit def SortedMapJava2Ts(values:SortedMap[JavaDate, Double]) : TimeSeries[JavaDouble] = 
//      new TimeSeries[java.lang.Double](JavaDouble.TYPE, values.map(q => (new qlDate(q._1), new JavaDouble(q._2))))
//
//	implicit def Sortedmap2Ts(m:SortedMap[qlDate, Double]) = new ConvertableSortedMap(m)
//	class ConvertableSortedMap(m:SortedMap[qlDate, Double]) {
//	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (q._1, new JavaDouble(q._2))))
//	}

	implicit def Map2Ts(m:scala.collection.Map[qlDate, Double]) = new ConvertableMap(m)
	class ConvertableMap(m:scala.collection.Map[qlDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (q._1, new JavaDouble(q._2))))
	}
	
//	implicit def JavaSortedmap2Ts(m:SortedMap[JavaDate, Double]) = new ConvertableJavaSortedMap(m)
//	class ConvertableJavaSortedMap(m:SortedMap[JavaDate, Double]) {
//	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (new qlDate(q._1), new JavaDouble(q._2))))
//	}
	
	
	implicit def JavaMap2Ts(m:scala.collection.Map[JavaDate, Double]) = new ConvertableJavaMap(m)
	class ConvertableJavaMap(m:scala.collection.Map[JavaDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (new qlDate(q._1), new JavaDouble(q._2))))
	}

	implicit def TimeSeriesToConvertibleTs(ts:TimeSeries[JavaDouble]) = new ConvertibleTimeSeries(ts)
	class ConvertibleTimeSeries(ts:TimeSeries[JavaDouble]) {
	  def toSortedMap = SortedMap(ts.mapValues(d => d.doubleValue).toSeq:_*)
	}
	
	implicit def Bond2RichBond(bond:qlBond) = new RichBond(bond)
	class RichBond(val bond:qlBond){
	  def bondprice(valuedate:qlDate, factory:DiscountCurveFactory) = BondPriceConstructor.getprice(bond, factory)
	  def bondprice(valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure):BondPrice 
			= BondPriceConstructor.getprice(bond, valuedate, fx, paramset, termstructure)
	}
	
	
}