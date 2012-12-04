package squantlib.util

import org.codehaus.jackson.JsonNode

object DisplayUtils {
  
	implicit def doubleToExtendedDouble(d:Double) = ExtendedDouble(d)
	case class ExtendedDouble(d:Double) {
	  def asPercent:String = "%.4f".format(d * 100.0) + "%"
	}
	
	implicit def doubleToExtendedDoubleOpt(d:Option[Double]) = ExtendedDoubleOpt(d)
	case class ExtendedDoubleOpt(d:Option[Double]) {
	  def asPercentOr(alternative:String, prefix:String, suffix:String):String = d match {
	    case Some(r) => prefix + "%.4f".format(r * 100.0) + "%" + suffix
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
