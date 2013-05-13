package squantlib.util

import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode
import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate}
import org.codehaus.jackson.`type`.TypeReference;


object JsonUtils {
  
	val jsonDateFormat = new java.text.SimpleDateFormat("y/M/d")
  
	implicit def jsonToExtendedJson(node:JsonNode) = ExtendedJson(node)
	
	case class ExtendedJson(node:JsonNode) {
	  
	  def hasName(name:String) = (node != null) && (node has name)
	  
	  def parseInt:Option[Int]= node match {
	    case null => None
	    case n if n.isInt => Some(n.getIntValue)
	    case n if n.isDouble => Some(n.getDoubleValue.round.toInt)
	    case n => FormulaParser.calculate(n.asText).collect{case d => d.round.toInt}
	  }
	  def parseInt(name:String):Option[Int] = if (hasName(name)) node.get(name).parseInt else None
	  
	  def parseDouble:Option[Double] = node match {
	    case null => None
	    case n if n.isNumber => Some(n.getDoubleValue)
		case n => FormulaParser.calculate(n.asText)
	  }
	  def parseDouble(name:String):Option[Double] = if (hasName(name)) node.get(name).parseDouble else None
	  
	  def parseString:Option[String] = Some(node.asText)
	  def parseString(name:String):Option[String] = if (hasName(name)) node.get(name).parseString else None

	  def parseDate:Option[qlDate] = try{Some(new qlDate(jsonDateFormat.parse(node.parseString.orNull)))} catch {case _:Throwable => None}
	  def parseDate(name:String):Option[qlDate] = 
	    if (name == null) None
	    else node.parseString(name) match {
	      case None => None
	      case Some(s) if !s.contains("/") => None
	      case Some(s) => try{Some(new qlDate(jsonDateFormat.parse(s)))} catch {case _:Throwable => None}	      
	    }
	  
	  def parseObject[T](constructor:Map[String, Any] => T):Option[T] = Some(constructor(node.parseValueFields))
	  def parseObject[T](name:String, constructor:Map[String, Any] => T):Option[T] = if (hasName(name)) Some(constructor(node.get(name).parseValueFields)) else None
	  
	  def parseList:List[JsonNode] = node match {
	    case n if n == null => List.empty
	    case n if n isArray => n.map(s => s).toList
	    case n if n.isNull => List.empty
	    case n => List(n)
	  }
	  def parseList(name:String):List[JsonNode] = if (hasName(name)) node.get(name).parseList else List.empty
	  
	  def parseIntList:List[Option[Int]] = parseList.map(_.parseInt)
	  def parseIntList(name:String):List[Option[Int]] = if (hasName(name)) node.get(name).parseIntList else List.empty
	  
	  def parseDoubleList:List[Option[Double]] = parseList.map(_.parseDouble)
	  def parseDoubleList(name:String):List[Option[Double]] = if (hasName(name)) node.get(name).parseDoubleList else List.empty
	  
	  def parseStringList:List[Option[String]] = parseList.map(_.parseString)
	  def parseStringList(name:String):List[Option[String]] = if (hasName(name)) node.get(name).parseStringList else List.empty
	  
	  def parseValueFields:Map[String, Any] = if (node == null) Map.empty
		  else node.getFieldNames.map(f => (f, node.get(f) match {
		    case n if n.isContainerNode => None
		    case n if n.isInt => n.parseInt
		    case n if n.isDouble => n.parseDouble
		    case n if n.isTextual => n.parseString
		    case n => n.parseString
		  })).collect{case (a, Some(b)) => (a, b)}.toMap
		  
	  def parseValueFields(name:String):Map[String, Any] = if (hasName(name)) node.get(name).parseValueFields else Map.empty
	  
	  def parseDoubleFields:Map[String, Double] = if (node == null) Map.empty
		  else node.getFieldNames.map(f => (f, node.get(f).parseDouble)).collect{case (a, Some(b)) => (a, b)}.toMap
		  
	  def parseDoubleFields(name:String):Map[String, Double] = if (hasName(name)) Map.empty
		  else node.get(name).getFieldNames.map(f => (f, node.get(f).parseDouble)).collect{case (a, Some(b)) => (a, b)}.toMap
		  
	  def parseStringFields:Map[String, String] = if (node == null) Map.empty
		  else node.getFieldNames.map(f => (f, node.get(f).parseString)).collect{case (a, Some(b)) => (a, b)}.toMap
		  
	  def parseStringFields(name:String):Map[String, String] = if (hasName(name)) Map.empty
		  else node.get(name).getFieldNames.map(f => (f, node.get(f).parseString)).collect{case (a, Some(b)) => (a, b)}.toMap
		  
	  
	}
	
	
	implicit def formulaToExtendedJson(formula:String) = JsonString(formula)
	
	case class JsonString(formula:String) {
	  
	  val mapper = new ObjectMapper
	  
	  def jsonNode:Option[JsonNode] = try { Some(mapper.readTree(formula)) } catch { case _:Throwable => None }
	  
	  def jsonNode(name:String):Option[JsonNode] = try { 
	    val node = mapper.readTree(formula).get(name)
	    if (node == null) None else Some(node)
	    } catch { case _:Throwable => None }
	    
	  def jsonArray:List[JsonNode] = jsonNode match {
	    case Some(n) if n isArray => n.getElements.toList
	    case Some(n) => List(n)
	    case _ => List.empty
	  }
	    
	  def jsonArray(name:String):List[JsonNode] = jsonNode(name) match {
	    case Some(n) if n isArray => n.getElements.toList
	    case Some(n) => List(n)
	    case _ => List.empty
	  }
	  
	  def jsonParser[T](f:JsonNode => Option[T]):Option[T] = jsonNode match { case Some(n) => f(n); case _ => None}
	  def jsonParserOrElse[T](f:JsonNode => T, alternative:T):T = jsonNode match { case Some(n) => f(n); case _ => alternative}
	  
	  def parseJson[T] = (new ObjectMapper).readValue(formula, new TypeReference[T]{})
	  
	  def parseJsonInt:Option[Int] = jsonParser(_.parseInt)
	  def parseJsonInt(name:String):Option[Int] = jsonParser(_.parseInt(name))
	  
	  def parseJsonDouble:Option[Double] = jsonParser(_.parseDouble)
	  def parseJsonDouble(name:String):Option[Double] = jsonParser(_.parseDouble(name))
	  	
	  def parseJsonString:Option[String] = jsonParser(_.parseString)
	  def parseJsonString(name:String):Option[String] = jsonParser(_.parseString(name))
	  
	  def parseJsonDate:Option[qlDate] = jsonParser(_.parseDate)
	  def parseJsonDate(name:String):Option[qlDate] = jsonParser(_.parseDate(name))
	  
	  def parseJsonObject[T](constructor:Map[String, Any] => T):Option[T] = jsonParser(_.parseObject(constructor))
	  def parseJsonObject[T](name:String, constructor:Map[String, Any] => T):Option[T] = jsonParser(_.parseObject(name, constructor))
	  
	  def parseJsonValueFields:Map[String, Any] = jsonParserOrElse(_.parseValueFields, Map.empty)
	  def parseJsonValueFields(name:String):Map[String, Any] = jsonParserOrElse(_.parseValueFields(name), Map.empty)
	  
	  def parseJsonIntList:List[Option[Int]] = jsonParserOrElse(_.parseIntList, List.empty)
	  def parseJsonIntList(name:String):List[Option[Int]] = jsonParserOrElse(_.parseIntList(name), List.empty)
	  
	  def parseJsonDoubleList:List[Option[Double]] = jsonParserOrElse(_.parseDoubleList, List.empty)
	  def parseJsonDoubleList(name:String):List[Option[Double]] = jsonParserOrElse(_.parseDoubleList(name), List.empty)
	  
	  def parseJsonStringList:List[Option[String]] = jsonParserOrElse(_.parseStringList, List.empty)
	  def parseJsonStringList(name:String):List[Option[String]] = jsonParserOrElse(_.parseStringList(name), List.empty)
	  
	  def parseJsonDoubleFields:Map[String, Double] = jsonParserOrElse(_.parseDoubleFields, Map.empty)
	  def parseJsonDoubleFields(name:String):Map[String, Double] = jsonParserOrElse(_.parseDoubleFields(name), Map.empty)
	  
	  def parseJsonStringFields:Map[String, String] = jsonParserOrElse(_.parseStringFields, Map.empty)
	  def parseJsonStringFields(name:String):Map[String, String] = jsonParserOrElse(_.parseStringFields(name), Map.empty)
	  
	}
	
	implicit def mapToExtendedMap(m:Map[String, Any]) = ParsingMap(m)
	
	case class ParsingMap(map:Map[String, Any]) {
	  
	  def getDouble(key:String):Option[Double] = map.get(key) match {
	    case Some(v:Double) => Some(v)
	    case Some(v:Int) => Some(v.toDouble)
	    case Some(v:String) => FormulaParser.calculate(v)
	    case _ => None
	  }
	  
	  def getInt(key:String):Option[Int] = map.get(key) match {
	    case Some(v:Int) => Some(v)
	    case Some(v:Double) => Some(v.round.toInt)
	    case Some(v:String) => FormulaParser.calculate(v).collect{case s => s.round.toInt}
	    case _ => None
	  }
	  
	  def getString(key:String):Option[String] = Some(map(key).toString)
	}


	
	def jsonString[T](map:Map[String, T]):String = {
	  val infoMap:java.util.Map[java.lang.String, T] = map
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}

} 


