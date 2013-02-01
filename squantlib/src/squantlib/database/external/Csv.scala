package squantlib.database.external

import scala.io.Source
import scala.util.parsing.combinator._
import scala.io.{Source, BufferedSource}
import java.util.{Date => JavaDate}
import java.text.SimpleDateFormat

import scala.io.Source
import scala.io.Codec
import java.nio.charset.CodingErrorAction.REPLACE

object CsvParser extends JavaTokenParsers {
    override def skipWhitespace = false
    
    def csvFile = rep(line <~ eol)
    def line = repsep(cell, ',')
    def cell = quotedCell | """[^,\n]*""".r ^^ (_.trim)
    def eol = "\n" | "\r\n"
    def quotedCell = '"' ~> quotedChars ~ rep(escapeQuotedChars) <~ '"' ^^ {case(x~xs) => x + xs.mkString}
    def quotedChars = """[^"]*""".r
    def escapeQuotedChars = "\"\"" ~> quotedChars ^^ ('"' + _)
}


case class Csv(data:List[List[String]]) {
  
  val titles = data.head.zipWithIndex.toMap
  val ids = data.map(_.head).zipWithIndex.toMap
  
  def apply(id:String, column:String):Option[String] = (ids.get(id), titles.get(column)) match {
    case (Some(i), Some(c)) => Some(data(i)(c))
    case _ => None
  }
  
  def apply(id:String, column:Int):Option[String] = (ids.get(id), column) match {
    case (Some(i), c) => Some(data(i)(c))
    case _ => None
  }
  
  private def tryParseDouble(s:String):Option[Double] = try{Some(s.toDouble)} catch {case _ => None}
  private def tryParseInt(s:String):Option[Int] = try{Some(s.toInt)} catch {case _ => None}
  
  def getDouble(row:String, column:String):Option[Double] = apply(row, column).flatMap{case c => tryParseDouble(c)}
  def getInt(row:String, column:String):Option[Int] = apply(row, column).flatMap{case c => tryParseInt(c)}
  
  private def tryParseDate(s:String):Option[JavaDate] = 
    try{ Some((new SimpleDateFormat("MM/dd/yyyy")).parse(s))} 
  	catch {case _ => try{ Some((new SimpleDateFormat("yyyy/MM/dd")).parse(s))} catch {case _ => None }}
  
  def getDate(row:String, column:String):Option[JavaDate] = apply(row, column).flatMap{case c => tryParseDate(c)}
  
  def keySet = ids.keySet
}

object Csv {
  
  var encoding:String = "Windows-31J" //"Shift_JIS" 
  
  implicit def codec = Codec(encoding).onUnmappableCharacter(REPLACE)    
  
  def using[A <% { def close():Unit }, B](s: A)(f: A=>Option[B]):Option[B] = {
	try f(s) catch {case _ => None} finally s.close()
  }
  
  def fromFile(location:String, blankRows:Int, blankColumns:Int):Option[Csv] = {
    using(Source.fromFile(location)) { source => {
      val parseResult = CsvParser.parseAll(CsvParser.csvFile, source.getLines.drop(blankRows).mkString("\n") + "\n")
      if (parseResult.isEmpty) None else Some(Csv(parseResult.get.map(l => l.drop(blankColumns))))
    }}
  }
  
  def fromURL(url:String, blankRows:Int, blankColumns:Int):Option[Csv] = {
    using(Source.fromURL(url)) { source => {
      val parseResult = CsvParser.parseAll(CsvParser.csvFile, source.getLines.drop(blankRows).mkString("\n") + "\n")
      if (parseResult.isEmpty) None else Some(Csv(parseResult.get.map(_.drop(blankColumns))))
    }}
  }
  
}



//package squantlib.database.external
//
//import scala.io.{Source, BufferedSource}
//import java.util.{Date => JavaDate}
//import java.text.SimpleDateFormat
//
//case class ExternalCSV(data:Map[String, Map[String, String]]) {
//  
//  def getLine(k:String):Option[Map[String, String]] = data.get(k)
//  
//  def apply(row:String, column:String):Option[String] = data.get(row).flatMap{case c => c.get(column)}
//  
//  private def tryParseDouble(s:String):Option[Double] = try{Some(s.toDouble)} catch {case _ => None}
//  private def tryParseInt(s:String):Option[Int] = try{Some(s.toInt)} catch {case _ => None}
//  
//  def getDouble(row:String, column:String):Option[Double] = apply(row, column).flatMap{case c => tryParseDouble(c)}
//  def getInt(row:String, column:String):Option[Int] = apply(row, column).flatMap{case c => tryParseInt(c)}
//  
//  private def tryParseDate(s:String):Option[JavaDate] = 
//    try{ Some((new SimpleDateFormat("MM/dd/yyyy")).parse(s))} 
//  	catch {case _ => try{ Some((new SimpleDateFormat("yyyy/MM/dd")).parse(s))} catch {case _ => None }}
//  
//  def getDate(row:String, column:String):Option[JavaDate] = apply(row, column).flatMap{case c => tryParseDate(c)}
//  
//  def keySet = data.keySet
//  
//}
//
//object ExternalCSV {
//  
//  def fromFile(location:String, titleRow:Int, key:AnyVal, encoding:String = "Shift_JIS"):Option[ExternalCSV] = {
//    val source = Source.fromFile(location, encoding)
//    apply(source, titleRow, key)
//  }
//  
//  def fromURL(url:String, titleRow:Int, key:AnyVal, encoding:String = "Shift_JIS"):Option[ExternalCSV] = {
//    val source = Source.fromURL(url, encoding)
//    apply(source, titleRow, key)
//  }
//  
//  def apply(source:BufferedSource, titleRow:Int, key:AnyVal):Option[ExternalCSV] = {
//    try {
//    	println("running source : " + source.toString)
//    	
//		val lines = source.getLines.toList
//    	println("title")
//		val title = lines(titleRow).split(',')
//		title.foreach(println)
//		  
//		val keyColumn = key match {
//		  case c:Int => c
//		  case c => title.indexOf(c.toString)
//		}
//		
//		println("cols : " + keyColumn)
//		  
//		val data = lines.drop(titleRow).map(line => {
//		  val ldata = line.split(',')
//		  val valueMap = (title zip ldata).toMap
//		  val k = ldata(keyColumn)
//		  (k, valueMap)
//		}).toMap
//		
//		println("data")
//		data.foreach(println)
//		  
//		source.close
//		
//		Some(ExternalCSV(data))
//	  }
//    catch { case _ => source.close; None}
//  }
//    
//}
