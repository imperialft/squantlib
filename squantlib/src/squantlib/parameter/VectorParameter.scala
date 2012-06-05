package squantlib.parameter

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import org.jquantlib.time.{ Date => JDate }
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.interpolation._

trait VectorParameter {
    var value : JDate => Double
}

class SplineParameter(values:SortedMap[JDate, Double]) {
    val valuelist = values
    val keysarray = valuelist.keySet.toArray
    val valarray = keysarray.map((d:JDate) => valuelist(d))
//    val splinefunction = new SplineInterpolator().interpolate(valuelist.keys.map((d:JDate) => d.serialNumber().toDouble).toArray, valuelist.values.toArray)
    val splinefunction = new SplineInterpolator().interpolate(keysarray.map((d:JDate)=>d.serialNumber().toDouble), valarray)
 
    var value : JDate => Double = (d:JDate) => {
      valuelist.size match {
        case m if m == 0 => Double.NaN
        case m if m == 1 => valuelist.first._2
        case _ =>
          d match {
            case dd if valuelist.lastKey.compareTo(d) < 0 => valuelist.last._2
            case dd if valuelist.firstKey.compareTo(d) > 0 => valuelist.head._2
            case _ => splinefunction.value(d.serialNumber().toDouble)
          }
      }
    }
}

class SplineXPParameter(values:SortedMap[JDate, Double]) {
	var valuelist = values
    valuelist ++= Map((values.firstKey.sub(30)) -> values.head._2)
    valuelist ++= Map((values.lastKey.add(30)) -> values.last._2)
    
    val keysarray = valuelist.keySet.toArray
    val valarray = keysarray.map((d:JDate) => valuelist(d))
    val splinefunction = new SplineInterpolator().interpolate(keysarray.map((d:JDate)=>d.serialNumber().toDouble), valarray)
    
    var value : JDate => Double = (d:JDate) => {
      valuelist.size match {
        case m if m == 0 => Double.NaN
        case m if m == 1 => valuelist.first._2
        case _ =>
          d match {
            case dd if valuelist.lastKey.compareTo(d) < 0 => valuelist.last._2
            case dd if valuelist.firstKey.compareTo(d) > 0 => valuelist.head._2
            case _ => splinefunction.value(d.serialNumber().toDouble)
          }
      }
    }
}

class LinearParameter(values:SortedMap[JDate, Double]) {
	var valuelist = values
    
    val keysarray = valuelist.keySet.toArray
    val valarray = keysarray.map((d:JDate) => valuelist(d))
    val splinefunction = new LinearInterpolator().interpolate(keysarray.map((d:JDate)=>d.serialNumber().toDouble), valarray)
    
    var value : JDate => Double = (d:JDate) => {
      valuelist.size match {
        case m if m == 0 => Double.NaN
        case m if m == 1 => valuelist.first._2
        case _ =>
          d match {
            case dd if valuelist.lastKey.compareTo(d) < 0 => valuelist.last._2
            case dd if valuelist.firstKey.compareTo(d) > 0 => valuelist.head._2
            case _ => splinefunction.value(d.serialNumber().toDouble)
          }
      }
    }
}

