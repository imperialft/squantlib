package squantlib.test

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable
import squantlib.parameter._
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit

object VectorInterpolationTest {
	
	def main(args:Array[String]) : Unit = {
	   
	  var testmap : TreeMap[JPeriod, Double] = TreeMap.empty
	  val vd = new JDate(1, 10, 2000)
	  val d1 = (new JDate(10, 10, 2000).ToPeriod(vd), 20.0)
	  val d2 = (new JDate(1, 1, 2002).ToPeriod(vd), 17.0)
	  val d3 = (new JDate(5, 2, 2005).ToPeriod(vd), 15.0)
	  val d4 = (new JDate(1, 10, 2012).ToPeriod(vd), 5.0)
	  
	  testmap ++= Map(d2)
	  testmap ++= Map(d4)
	  testmap ++= Map(d1)
	  testmap ++= Map(d3)
	  
	  println("Input")
	  for (d <- testmap.keySet) { println(d.toString() + ":" + testmap(d).toString())}
	  
	  var flat = new FlatVector(vd, Map(d1))
	  var linear = new LinearNoExtrapolation(vd, testmap)
	  var spline = new SplineNoExtrapolation(vd, testmap, 2)
	  var spline2 = new SplineEExtrapolation(vd, testmap, 1)
	  
	  var mindate = new JDate(5, 5, 2000)
	  val testcase = 100
	  val testperiod = 3
	   
	  var inputset = for (i <- 0 to (testcase * testperiod) if i % testperiod == 0) yield new JPeriod(i, TimeUnit.Months)
	  
	  println("Output")
	  println("Flat : Linear : Spline : Spline+Points")
	  inputset.foreach( (d:JPeriod) => { println(d.toString() + "  " + flat.value(d)+ "  " + linear.value(d) + " " + spline.value(d) + " " + spline2.value(d))})
	  println("max defined date" + flat.maxdate.toString() + " " + linear.maxdate.toString() + " " + spline.maxdate.toString() + " " + spline2.maxdate.toString())
    }
}