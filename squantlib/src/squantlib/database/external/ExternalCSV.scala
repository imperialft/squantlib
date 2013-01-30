package squantlib.database.external

import scala.io.{Source, BufferedSource}
import java.util.{Date => JavaDate}
import java.text.SimpleDateFormat

case class ExternalCSV(data:Map[String, Map[String, String]]) {
  
  def getLine(k:String):Option[Map[String, String]] = data.get(k)
  
  def apply(row:String, column:String):Option[String] = data.get(row).flatMap{case c => c.get(column)}
  
  private def tryParseDouble(s:String):Option[Double] = try{Some(s.toDouble)} catch {case _ => None}
  private def tryParseInt(s:String):Option[Int] = try{Some(s.toInt)} catch {case _ => None}
  
  def getDouble(row:String, column:String):Option[Double] = apply(row, column).flatMap{case c => tryParseDouble(c)}
  def getInt(row:String, column:String):Option[Int] = apply(row, column).flatMap{case c => tryParseInt(c)}
  
  private def tryParseDate(s:String):Option[JavaDate] = 
    try{ Some((new SimpleDateFormat("MM/dd/yyyy")).parse(s))} 
  	catch {case _ => try{ Some((new SimpleDateFormat("yyyy/MM/dd")).parse(s))} catch {case _ => None }}
  
  def getDate(row:String, column:String):Option[JavaDate] = apply(row, column).flatMap{case c => tryParseDate(c)}
  
  def keySet = data.keySet
  
}

object ExternalCSV {
  
  def fromFile(location:String, titleRow:Int, key:AnyVal, encoding:String = "Shift_JIS"):Option[ExternalCSV] = {
    val source = Source.fromFile(location, encoding)
    apply(source, titleRow, key)
  }
  
  def fromURL(url:String, titleRow:Int, key:AnyVal, encoding:String = "Shift_JIS"):Option[ExternalCSV] = {
    val source = Source.fromURL(url, encoding)
    apply(source, titleRow, key)
  }
  
  def apply(source:BufferedSource, titleRow:Int, key:AnyVal):Option[ExternalCSV] = {
    try {
    	println("running source : " + source.toString)
    	
		val lines = source.getLines.toList
    	println("title")
		val title = lines(titleRow).split(',')
		title.foreach(println)
		  
		val keyColumn = key match {
		  case c:Int => c
		  case c => title.indexOf(c.toString)
		}
		
		println("cols : " + keyColumn)
		  
		val data = lines.drop(titleRow).map(line => {
		  val ldata = line.split(',')
		  val valueMap = (title zip ldata).toMap
		  val k = ldata(keyColumn)
		  (k, valueMap)
		}).toMap
		
		println("data")
		data.foreach(println)
		  
		source.close
		
		Some(ExternalCSV(data))
	  }
    catch { case _ => source.close; None}
  }
    
}
