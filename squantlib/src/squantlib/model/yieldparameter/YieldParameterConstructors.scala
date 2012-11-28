package squantlib.model.yieldparameter

import squantlib.model.yieldparameter._
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}

object YieldParameterConstructors {
  
	implicit def Map2ConvertibleMap(values:Map[qlPeriod, Double]) = new ConvertibleMap(values)
  
	class ConvertibleMap(val values:Map[qlPeriod, Double]){
	  def toFlatVector(valuedate:qlDate):FlatVector = 
	    if (values.size != 1) null 
	    else new FlatVector(valuedate, values)
	  
	  def toLinearNoExtrapolation(valuedate:qlDate):YieldParameter = 
	    if (values.size > 1) new LinearNoExtrapolation(valuedate, values) 
	    else toFlatVector(valuedate)
	  
	  def toSplineEExtrapolation(valuedate:qlDate, extrapoints:Int = 0) = 
	    if (values.size > 2) new SplineEExtrapolation(valuedate, values, extrapoints) 
	    else toLinearNoExtrapolation(valuedate)
	    
	  def toSplineNoExtrapolation(valuedate:qlDate, extrapoints:Int = 0) = 
	    if (values.size > 2) new SplineNoExtrapolation(valuedate, values, extrapoints) 
	    else toLinearNoExtrapolation(valuedate)
	  
	}

}