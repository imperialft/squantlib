package squantlib.util

import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.JsonNode


object JsonUtils {
  
	implicit def jsonToExtendedJson(node:JsonNode) = ExtendedJson(node)
	
	case class ExtendedJson(node:JsonNode) {
	  
	  def parseJsonDouble:Option[Double] = node match {
	    case n if n.isNumber => Some(n.getDoubleValue)
		case n if n.getTextValue.trim.endsWith("%") => 
		  try {Some(n.getTextValue.trim.dropRight(1).toDouble / 100)} 
		  catch { case e:Exception => println(e.getMessage) ; None}
		case _ => None
	  }
	  
	  def parseJsonDouble(name:String):Option[Double] = if (node has name) node.get(name).parseJsonDouble else None
	  
	  def parseJsonString(name:String):String = if (node has name) node.get(name).getTextValue else null
	    
	}
	
	
	implicit def formulaToExtendedJson(formula:String) = JsonString(formula)
	
	case class JsonString(formula:String) {
	  
	  val mapper = new ObjectMapper
	  
	  def jsonNode:Option[JsonNode] = try { Some(mapper.readTree(formula)) } catch { case _ => None }
	  
	  def jsonNode(name:String):Option[JsonNode] = try { 
	    val node = mapper.readTree(formula).get(name)
	    if (node == null) None else Some(node)
	    } catch { case _ => None }
	  
	  def parseJsonDouble:Option[Double] = 
	    try { mapper.readTree(formula).parseJsonDouble }
	  	catch { case _ => None }
	  	
	  def parseJsonDouble(name:String):Option[Double] = 
	    try { mapper.readTree(formula).get(name).parseJsonDouble }
	  	catch { case _ => None }
	  	
	  def parseJsonString(name:String):String = 
	    try { mapper.readTree(formula).get(name).getTextValue }
	  	catch { case _ => null }
	  	
	}
  
} 


