package squantlib.database.external

import scala.io.Source

case class ExternalCSV(data:Map[String, Map[String, String]]) {
  
  def getLine(k:String):Option[Map[String, String]] = data.get(k)
  
  def apply(row:String, column:String):Option[String] = data.get(row).flatMap{case c => c.get(column)}
  
  def tryParseDouble(s:String):Option[Double] = try{Some(s.toDouble)} catch {case _ => None}
  
  def getDouble(row:String, column:String):Option[Double] = apply(row, column).flatMap{case c => tryParseDouble(c)}
  
  def keySet = data.keySet
  
}

object ExternalCSV {
  
  def apply(url:String, titleRow:Int, key:AnyVal, encoding:String = "Shift_JIS"):Option[ExternalCSV] = {
    try {
		val source = Source.fromURL(url, encoding)
		val lines = source.getLines.toList
		val title = lines(titleRow).split(',')
		  
		val keyColumn = key match {
		  case c:Int => c
		  case c => title.indexOf(c.toString)
		}
		  
		val data = lines.drop(titleRow).map(line => {
		  val ldata = line.split(',')
		  val valueMap = (title zip ldata).toMap
		  val k = ldata(keyColumn)
		  (k, valueMap)
		}).toMap
		  
		source.close
		
		Some(ExternalCSV(data))
	  }
    catch { case _ => None}
  }
    
}
