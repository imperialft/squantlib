package squantlib.test

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable

import org.jquantlib.time.{ Date => JDate }

object testvector {
	import squantlib.parameter._ 
	import org.jquantlib.time.{ Date => JDate }
	
	def main(args:Array[String]) : Unit = {
	  
	  val d1 = new JDate(10, 10, 2000)
	  val d2 = new JDate(1, 1, 2002)
	  val d3 = new JDate(5, 2, 2005)
	  val d4 = new JDate(15, 3, 2012)
	  
	  val v1 = 1.0
	  val v2 = 1.5
	  val v3 = 10.2
	  val v4 = 11.5
	  
	  var testmap : TreeMap[JDate, Double] = TreeMap.empty
	  testmap ++= Map(d2 -> v2)
	  testmap ++= Map(d4 -> v4)
	  testmap ++= Map(d1 -> v1)
	  testmap ++= Map(d3 -> v3)
	  
	  println("Input")
	  for (d <- testmap.keySet) { println(d.shortDate().toString() + ":" + testmap(d).toString())}
//	  testmap.foreach((d:JDate, v:Double) =>  { println(d.shortDate().toString())}) 
	  
	  var linear = new LinearParameter(testmap)
	  var spline = new SplineParameter(testmap)
	  var spline2 = new SplineXPParameter(testmap)
	  
	  var mindate = new JDate(5, 5, 2000)
	  val testcase = 100
	  val testperiod = 50
	  
	  var inputset = new Array[JDate](testcase)
	  for (i <- 0 to testcase-1) { inputset(i) = mindate.add(i * testperiod)}
	  
//	  val inputset = Array (new JDate(5, 5, 1995), new JDate(5, 2, 2005), new JDate(5, 5, 2007), new JDate(5, 5, 2009), new JDate(5, 5, 2022))
	  
	  println("Output")
	  println("Spline : Spline+Points : Linear")
	   inputset.foreach( (d:JDate) => { println(d.shortDate().toString() + "  " + spline.value(d) + " " + spline2.value(d) + " " + linear.value(d))})
	  
	  
    }
}