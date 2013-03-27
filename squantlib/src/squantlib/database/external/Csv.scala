package squantlib.database.external

import scala.util.parsing.combinator._
import scala.io.{Source, BufferedSource, Codec}
import java.util.{Date => JavaDate}
import java.text.SimpleDateFormat
import java.nio.charset.CodingErrorAction.REPLACE
import scala.collection.breakOut

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
  
  val titles:Map[String, Int] = data.head.zipWithIndex.toMap
  val ids = data.tail.map(_.head).zipWithIndex.toMap
  
  def apply(id:String, column:String):Option[String] = (ids.get(id), titles.get(column)) match {
    case (Some(i), Some(c)) => Some(data(i+1)(c))
    case _ => None
  }
  
  def apply(id:String, column:Int):Option[String] = (ids.get(id), column) match {
    case (Some(i), c) => Some(data(i+1)(c))
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


