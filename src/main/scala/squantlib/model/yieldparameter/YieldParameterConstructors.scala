package squantlib.model.yieldparameter

import squantlib.model.yieldparameter._
import org.jquantlib.time.{Period => qlPeriod, Date => jDate}
import squantlib.util.Date

object YieldParameterConstructors {
  
	implicit def Map2ConvertibleMap(values:Map[qlPeriod, Double]) = new ConvertibleMap(values)
  
	class ConvertibleMap(val values:Map[qlPeriod, Double]){
	  def toFlatVector(valuedate:Date):FlatVector = 
	    if (values.size != 1) null 
	    else FlatVector(valuedate, values.head._2)
	  
	  def toLinearNoExtrapolation(valuedate:Date):YieldParameter = 
	    if (values.size > 1) LinearNoExtrapolation(valuedate, values) 
	    else toFlatVector(valuedate)
	  
	  def toSplineEExtrapolation(valuedate:Date, extrapoints:Int = 0) = 
	    if (values.size > 2) SplineEExtrapolation(valuedate, values, extrapoints) 
	    else toLinearNoExtrapolation(valuedate)
	    
	  def toSplineNoExtrapolation(valuedate:Date, extrapoints:Int = 0) = 
	    if (values.size > 2) SplineNoExtrapolation(valuedate, values, extrapoints) 
	    else toLinearNoExtrapolation(valuedate)
	  
	}

}