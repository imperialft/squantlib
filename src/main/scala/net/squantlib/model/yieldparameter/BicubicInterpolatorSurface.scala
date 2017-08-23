package net.squantlib.model.yieldparameter

import scala.collection.immutable.SortedMap
import org.jquantlib.time.{Period => qlPeriod}
import org.apache.commons.math3.analysis.interpolation._
import org.apache.commons.math3.analysis.function.{Log, Exp}
import net.squantlib.util.Date

case class BicubicInterpolatorSurface(
  var valuedate:Date,
  xs:Array[Double],
  ys:Array[Double],
  vs:Array[Array[Double]]
) extends YieldParameter3D {
	
  val minValue = vs.flatten.min
  val maxValue = vs.flatten.max
  val minX = xs.min
  val minY = ys.min
  val maxX = xs.max
  val maxY = ys.max
	
  def value(x1:Double, y1:Double):Double = 
  {
    val rangedX = math.max(minX, math.min(maxX, x1))
    val rangedY = math.max(minY, math.min(maxY, y1))
    math.max(minValue, math.min(maxValue, interpolation(rangedX, rangedY)))
  }

  def interpolation(x1:Double, x2:Double) = sphere.value(x1, x2)

  val sphere = {
    new PiecewiseBicubicSplineInterpolatingFunction(xs, ys, vs)
  }    

  def shifted(shift:(Double, Double, Double) => Double):BicubicInterpolatorSurface = {
    new BicubicInterpolatorSurface(valuedate, xs, ys, vs.zip(xs).map{case (vv, x) => vv.zip(ys).map{case (v, y) => shift(x, y, v)}})
  }
    
}


object BicubicInterpolatorSurface{
  
  def construct(valuedate:Date, values: => Map[(Double, Double), Double]):Option[BicubicInterpolatorSurface] = {
    val roundUnit = 10000.0
    val roundedValues = values.map{case ((x, y), v) => ((Math.round(x * roundUnit), Math.round(y * roundUnit)), v)}
    
    val xs:Array[Long] = roundedValues.groupBy{case ((x, y), v) => x}.filter{case (k, vs) => vs.size >= 3}.keySet.toArray.sorted
    val ys:Array[Long] = roundedValues.map{case ((x, y), v) => y}.toSet.toArray.sorted
    
//    println("xs")
//    xs.foreach(println)
//    
//    println("ys")
//    ys.foreach(println)
    
    val vs:Array[Array[Double]] = xs.map(x => {
      val coords = roundedValues.filter{case ((xx, yy), vv) => xx == x}.map{case ((xx, yy), vv) => (yy.toDouble, vv)}.toArray.sortBy{case (xx, yy) => xx}
      val xInterp = new SplineInterpolator().interpolate(coords.map{case (xx, yy) => xx}, coords.map{case (xx, yy) => yy})
      val minY = coords.map{case (xx, yy) => xx}.min
      val maxY = coords.map{case (xx, yy) => xx}.max
      ys.map(y => xInterp.value(math.max(minY, math.min(maxY, y)))).toArray
    })

//    println("vs")
//    vs.foreach(vv => println(vv.mkString(" / ")))
    
    if (xs.size >= 3 && ys.size >= 3) {
      Some(BicubicInterpolatorSurface(valuedate, xs.map(xx => xx.toDouble / roundUnit), ys.map(yy => yy.toDouble / roundUnit), vs))
    } else None
  }
    
}
  
  
  
  
  
  
  
  
  