package net.squantlib.util

import scala.language.postfixOps
import scala.language.implicitConversions
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{ObjectNode, ArrayNode}
import scala.collection.JavaConversions._
import org.codehaus.jackson.`type`.TypeReference;
import java.util.{List => JavaList, Map => JavaMap}
import java.lang.{String => JavaString}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._

object JsonUtils {
  
  val mapper = new ObjectMapper
  
  def jsonDateFormat = new java.text.SimpleDateFormat("y/M/d")
  
  def newObjectNode = mapper.createObjectNode
  
  def newArrayNode = mapper.createArrayNode
  
  implicit def jsonToExtendedJson(node:JsonNode) = ExtendedJson(node)
  
  def mapToJson[T <: Any, U <: Any](map:Map[T, U]):JsonNode = mapper.valueToTree(scalaAnyMapToJavaMap(map))
  
  private def isNumber(s:Any):Boolean = try {val t = s.toString.toDouble; true} catch {case e:Throwable => false}
  
  def listToJson(list:List[Any]):JsonNode = try{
    val elems = 
      if (list == null) List.empty[String]
      else if (list.forall(v => v != null && isNumber(v))) list 
      else list.map(v => if (v == null) null else v.toString)
    mapper.valueToTree(scalaListToJavaList(elems))
  } catch {case e:Throwable => 
    errorOutput("failed to create json node from : " + list.toString)
    newArrayNode}
   
  case class ExtendedJson(node:JsonNode) {
    
    def hasName(name:String):Boolean = (node != null) && (node has name)
    
    def getOption(name:String):Option[JsonNode] = if(node == null || !node.has(name)) None else Some(node.get(name))
    
    def parseInt:Option[Int]= node match {
      case null => None
      case n if n.isInt => Some(n.getIntValue)
      case n if n.isDouble => Some(n.getDoubleValue.round.toInt)
      case n => FormulaParser.calculate(n.asText).collect{case d => d.round.toInt}
    }
    
    def parseInt(name:String):Option[Int] = if (hasName(name)) node.get(name).parseInt else None
    
    def parseDouble:Option[Double] = node match {
      case n if n == null || n.isNull => None
      case n if n.isNumber => Some(n.getDoubleValue)
      case n => FormulaParser.calculate(n.asText)
    }
    
    def parseDouble(name:String):Option[Double] = if (hasName(name)) node.get(name).parseDouble else None
    
    def parseString:Option[String] = Some(node.asText)
    
    def parseString(name:String):Option[String] = if (hasName(name)) node.get(name).parseString else None
    
    
    def ignoreErr[T](f: =>T):T = {
      val emptyStream = new java.io.PrintStream(new java.io.OutputStream {override def write(b:Int){}})
      System.setErr(emptyStream)
      val r = f
      System.setErr(System.err)
      r
    }

    def parseDate:Option[Date] = try{
      Some(Date(jsonDateFormat.parse(node.parseString.orNull)))
    } catch {case _:Throwable => None}
    
    def parseDate(name:String):Option[Date] = 
      if (name == null) None
      else node.parseString(name) match {
        case None => None
        case Some(s) if s.size < 8 || !s.contains("/") => None
        case Some(s) => ignoreErr {
           try{ Some(Date(jsonDateFormat.parse(s)))} 
          catch { case e:Throwable => errorOutput(s + " could not be parsed"); e.printStackTrace; None}
        }
      }
    
    def parseObject[T](constructor:Map[String, Any] => T):Option[T] = Some(constructor(node.parseValueFields))
    def parseObject[T](name:String, constructor:Map[String, Any] => T):Option[T] = if (hasName(name)) Some(constructor(node.get(name).parseValueFields)) else None
    
    def parseList:List[JsonNode] = node match {
      case n if n == null => List.empty
      case n if n isArray => n.toList
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
  
    def parseAllFieldMap:Map[String, List[Any]] = parseAllFieldMap(5)
    
    def parseAllFieldMap(parent:String, maxDepth:Int):Map[String, List[Any]] = node.getOption(parent) match {
      case Some(n) => n.parseAllFieldMap
      case None => Map.empty
    }
    
    def parseAllFieldMap(maxDepth:Int):Map[String, List[Any]] = parseAllFieldMapRec(node, None, maxDepth)
    
    private def parseAllFieldMapRec(n:JsonNode, key:Option[String], depth:Int):Map[String, List[Any]] = n match {
      case nn if nn == null || depth < 0 => Map.empty
      case nn if nn.isArray => 
        val tempmap = nn.map(nnn => parseAllFieldMapRec(nnn, key, depth - 1))
        if (tempmap.isEmpty) Map.empty else tempmap.reduce(addMaps)
      case nn if nn.isObject => 
        val tempmap = nn.getFields.map(nnn => parseAllFieldMapRec(nnn.getValue, Some(nnn.getKey), depth - 1))
        if (tempmap.isEmpty) Map.empty else tempmap.reduce(addMaps)
      case nn if nn.isTextual => Map(key.getOrElse("") -> List(nn.parseDate.getOrElse(nn.asText)))
      case nn if nn.isNumber => Map(key.getOrElse("") -> List(nn.asDouble))
      case nn => Map.empty
    }
      
    private def addMaps(a:Map[String, List[Any]], b:Map[String, List[Any]]):Map[String, List[Any]] = 
      a.filter(aa => !b.contains(aa._1)) ++ b.filter(bb => !a.contains(bb._1)) ++ (a.keySet & b.keySet).map(ab => (ab -> (a(ab) ++ b(ab))))
    
    def parseChild[A](parent:String, f:JsonNode => A):Option[A] = getOption(parent).collect{case nn => f(nn)}
      
    def toJsonString:String = mapper.writeValueAsString(node)
    
  }
  
  
  implicit def formulaToExtendedJson(formula:String) = JsonString(formula)
  
  case class JsonString(formula:String) {
    
    def jsonNode:Option[JsonNode] = try { Some(mapper.readTree(formula)) } catch { case _:Throwable => None }
    
    def objectNode:Option[ObjectNode] = try { Some(mapper.readTree(formula).asInstanceOf[ObjectNode]) } catch { case _:Throwable => None }
    
    def arrayNode:Option[ArrayNode] = try { Some(mapper.readTree(formula).asInstanceOf[ArrayNode]) } catch { case _:Throwable => None }
    
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
    
    def parseJsonDate:Option[Date] = jsonParser(_.parseDate)
    def parseJsonDate(name:String):Option[Date] = jsonParser(_.parseDate(name))
    
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
  
  def scalaStringtoJavaString(s:String):JavaString = s
  
  def scalaMapToJavaMap[T <: Any](m:Map[String, T]):JavaMap[JavaString, T] = m
  def scalaAnyMapToJavaMap[T <: Any, U <: Any](m:Map[T, U]):JavaMap[T, U] = m
  
  def scalaListToJavaList[T <: Any](m:Seq[T]):JavaList[T] = m
    
  def toJavaCollection(colset:Any):Any = colset match {
    case c:Map[_, _] => scalaAnyMapToJavaMap(c.map{case (k, v) => (k, toJavaCollection(v))})
    case c:Traversable[Any] => scalaListToJavaList(c.toSeq.map(cc => toJavaCollection(cc)))
    case c:String => val js:JavaString = c; js
    case c => c
  }
  
  def jsonString(obj:Any):String = try {(new ObjectMapper).writeValueAsString(toJavaCollection(obj))} catch {case e:Throwable => ""}
  

} 


