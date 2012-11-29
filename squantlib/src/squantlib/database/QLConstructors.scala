package squantlib.database

import squantlib.database.schemadefinitions.{ Bond => dbBond, CDSParameter, BondPrice, RateFXParameter}
import squantlib.model.rates._
import squantlib.model.CurveFactory
import squantlib.database.objectconstructor.BondPrice
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
	  def toLiborDiscountCurves:Iterable[RateCurve] = LiborDiscountCurve(parameters)
	  def toFXDiscountCurves:Iterable[FXCurve] = FXDiscountCurve(parameters)
	  def toDiscountCurves:Iterable[DiscountableCurve] = toLiborDiscountCurves ++ toFXDiscountCurves
	} 

	
	implicit def CDSParameter2ParamSet(params:Traversable[CDSParameter]) = new CDSParameterSet(params)
	class CDSParameterSet(val cdsparameters:Traversable[CDSParameter]){
	  def toCDSCurves:Iterable[CDSCurve] = CDSCurve.getcurves(cdsparameters)
	}

	implicit def Map2Ts(m:scala.collection.Map[qlDate, Double]) = new ConvertableMap(m)
	class ConvertableMap(m:scala.collection.Map[qlDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (q._1, new JavaDouble(q._2))))
	}
	
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
	  def bondprice(factory:CurveFactory) = BondPrice(bond, factory)
	  def bondprice(valuedate:qlDate, factory:CurveFactory) = BondPrice(bond, factory)
	  def bondprice(valuedate:qlDate, fx:Double, paramset:String, termstructure:YieldTermStructure):BondPrice 
			= BondPrice(bond, valuedate, fx, paramset, termstructure)
			
	  def isPriceable:Boolean = {
	    val estream = new java.io.PrintStream(new java.io.OutputStream{
	    	override def write(b:Int) = {}})
	    val err = System.err
	    System.setErr(estream)
	    val result = try { val p = bond.dirtyPrice; !p.isNaN } catch { case _ => false }
	    System.setErr(err)
	    result
	  }
	}
	
	
}


