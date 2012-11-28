package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}


object DisplayUtils {
  
	implicit def doubleToExtendedDouble(d:Double) = ExtendedDouble(d)
	case class ExtendedDouble(d:Double) {
	  def asPercent:String = "%.4f".format(d * 100) + "%"
	}
	
	implicit def doubleToExtendedDoubleOpt(d:Option[Double]) = ExtendedDoubleOpt(d)
	case class ExtendedDoubleOpt(d:Option[Double]) {
	  def asPercentOr(alternative:String, prefix:String, suffix:String):String = d match {
	    case Some(r) => prefix + "%.4f".format(r * 100) + "%" + suffix
	    case None => alternative
	  }
	  def asPercentOr(alternative:String):String = asPercentOr(alternative, "", "")
	  def asPercent = asPercentOr("")
	  
	  def asDoubleOr(alternative:String, prefix:String, suffix:String):String = d match {
	    case Some(r) => prefix + "%.4f".format(r) + suffix
	    case None => alternative
	  }
	  def asDoubleOr(alternative:String):String = asDoubleOr(alternative, "", "")
	  def asDouble = asDoubleOr("")
	}
	   
	implicit def stringToExtendedString(s:String) = ExtendedString(s)
	case class ExtendedString(s:String) {
	  def or(t:String) = if (s == null) t else s
	  
	  def parseDouble:Option[Double] = s.trim match {
	    case n if n.isEmpty => None
		case n if n.endsWith("%") => try {Some(n.dropRight(1).toDouble / 100)} catch { case _ => None}
		case n => try {Some(n.toDouble)} catch { case _ => None}
	  }	  
	  
	  def textOr(t:String) = if (s == null) t else s
	}
	
	implicit def nodeToExtendedNode(node:JsonNode) = ExtendedNode(node)
	case class ExtendedNode(node:JsonNode) {
	  def textOr(t:String) = if (node == null) t else node.getTextValue
	}
}

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
	  def jsonnode:Option[JsonNode] = try { Some(mapper.readTree(formula)) } catch { case _ => None }
	  def jsonnode(name:String):Option[JsonNode] = try { Some(mapper.readTree(formula).get(name)) } catch { case _ => None }
	  
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


object FormulaParser {
  import DisplayUtils._

  	def parseList(cashflow:String):List[(Map[Set[String], Double], Option[Double], Option[Double])] = 
	  cashflow.split(",").map(parse).toList
  	
  /** 
   * Parse a linear formula string into sum of named variables
   * returns tuple containing
   * 1) Cashflow as map(variable names -> leverage)
   * 2) Floor (ie. minimum amount)
   * 3) Cap (ie. maximum amount)
   */
    def parse(cashflow:String):(Map[Set[String], Double], Option[Double], Option[Double]) = {
	  if (cashflow.isEmpty) return (Map.empty, None, None)
      
      var coeff:mutableMap[Set[String], Double] = mutableMap.empty
      var cap:Option[Double] = None
      var floor:Option[Double] = None
      
      var rateformula = (if(cashflow.trim.head == '-') "0" else "") + replacestr(cashflow, 
          Map(" " -> "", "\r" -> "", "\n" -> "", "-" -> "+-1*", ">" -> "+>", "<" -> "+<"))

      rateformula.split('+').withFilter(!_.isEmpty).foreach{ 
          case s if (s.head == '<') => {
	        cap = s.tail.parseDouble.collect {
	            case v => cap match {
	              case None => v
	              case Some(c) => c.min(v)
	      }}}
          
          case s if (s.head == '>') => {
            floor = s.tail.parseDouble.collect {
              case v => floor match {
	          	case None => v
	          	case Some(f) => f.max(v)
	      }}}
          
          case s => {
            val splitmult = s.replace("/", "*/").split('*').filter(!_.isEmpty)
		    var c = 1.00
		    var varnames = Set.empty[String]
		    
		    splitmult.foreach{ 
		        case t if (t.head == '/') => {
		          val valuepart = t.tail
		          valuepart.parseDouble match {
		            case Some(v) => { c /= v }
		            case None => {
		              if (isFX(valuepart)) varnames += flipFX(valuepart) 
		              else varnames += t
		        }}}
		        
		        case t => {
		          t.parseDouble match {
		            case Some(v) => c = c * v
		            case None => varnames += t
		        }}
		      }
		        
		      if (coeff.contains(varnames)) coeff(varnames) += c
		      else coeff.update(varnames, c)
	        }
	     }

      (Map(coeff.toSeq:_*), floor, cap)
    }
    
//  /**
//   * Parse string into double. Returns Null if string is not a number.
//   */
//	def parseDouble(s: String):Option[Double] = try { Some(s.toDouble) } catch { case _ => None }
	
	private def isFX(s:String):Boolean = 
	  (s != null && s.length == 6 && Currencies.contains(s.substring(0, 3)) && Currencies.contains(s.substring(3, 6)))
	  
	private def flipFX(s:String):String = s.substring(3, 6) + s.substring(0, 3)

	private def replacestr(s:String, replacef:Map[String, String]):String = {
	  var resultstring = s
	  replacef.keySet.foreach{r => {
	    resultstring = resultstring.replace(r, replacef(r))
	  }}
	  resultstring
	}
}
