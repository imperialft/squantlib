package squantlib.util

import org.codehaus.jackson.JsonNode

object DisplayUtils {
  
	implicit def stringToExtendedString(s:String) = ExtendedString(s)
	case class ExtendedString(s:String) {
	  def or(t:String) = if (s == null) t else s
	  
	  def parseDouble:Option[Double] = s.trim match {
	    case n if n.isEmpty => None
		case n if n.endsWith("%") => try {Some(n.dropRight(1).toDouble / 100)} catch { case _:Throwable => None}
		case n => try {Some(n.toDouble)} catch { case _:Throwable => None}
	  }	  
	  
	  def textOr(t:String) = if (s == null) t else s
			  
	  def trimZeros:String = {
	    def trimAcc(d:String):String = d match {
	      case d if d.isEmpty => "0"
	      case d if (d takeRight 2) == ".0" => d
	      case d if (d takeRight 1) == "." => trimAcc(d dropRight 1)
		  case d if (d takeRight 1) == "0" && (d contains ".") => trimAcc(d dropRight 1)
		  case d => d
		}
		trimAcc(s)
	  }
	  
	  def oneByteString = {
	    var result = s
	    result = result.map{
	      case c if c >= 'ａ' && c <= 'ｚ' => (c - 'ａ' + 'a').toChar
	      case c if c >= 'Ａ' && c <= 'Ｚ' => (c - 'Ａ' + 'A').toChar
	      case c if c >= '０' && c <= '９' => (c - '０' + '0').toChar
	      case '　' => ' '
	      case '（' => '('
	      case '）' => ')'
	      case '\n' => ' '
	      case '／' => '/'
	      case '．' => '.'
	      case '?' => ' '
	      case '＆' => '&'
	      case '，' => ','
	      case '、' => ','
	      case c => c
	    }
	    result
	  }
	  
	}
	
	implicit def doubleToExtendedDouble(d:Double) = ExtendedDouble(d)
	case class ExtendedDouble(d:Double) {
	  def asPercent:String = "%.4f".format(d * 100.0).trimZeros + "%"
	  def asPercent(decimals:Int):String = ("%." + decimals + "f").format(d * 100.0).trimZeros + "%"
	  
	  def asDouble:String = "%,.4f".format(d).trimZeros
	  def asDouble(decimals:Int):String = ("%,." + decimals + "f").format(d).trimZeros
	}
	
	implicit def doubleToExtendedDoubleOpt(d:Option[Double]) = ExtendedDoubleOpt(d)
	case class ExtendedDoubleOpt(d:Option[Double]) {
	  
	  def asPercentOr(alternative:String, prefix:String, suffix:String):String = d match {
	    case Some(r) => prefix + "%.4f".format(r * 100.0).trimZeros + "%" + suffix
	    case None => alternative
	  }
	  def asPercentOr(alternative:String):String = asPercentOr(alternative, "", "")
	  def asPercent = asPercentOr("")
	  
	  def asDoubleOr(alternative:String, prefix:String, suffix:String):String = d match {
	    case Some(r) => prefix + "%,.4f".format(r).trimZeros + suffix
	    case None => alternative
	  }
	  def asDoubleOr(alternative:String):String = asDoubleOr(alternative, "", "")
	  def asDouble = asDoubleOr("")
	}
	
	implicit def nodeToExtendedNode(node:JsonNode) = ExtendedNode(node)
	case class ExtendedNode(node:JsonNode) {
	  def textOr(t:String) = if (node == null) t else node.asText
	}
	
	def linearFormula(coeff:Option[Double], varname:String, constant:Option[Double]):String = {
	  val coeffstr = coeff match {
	    case None => ""
	    case Some(c) if math.round(c * 1000000) == 1000000 => varname
	    case Some(c) => c.asDouble + "*" + varname
	  }
	  
	  val conststr = constant match {
	    case None => ""
	    case Some(c) if math.abs(math.round(c * 1000000)) < 1.0 => ""
	    case Some(c) => c.asPercent
	  }
	  
	  val addsign = if (coeffstr.isEmpty || conststr.isEmpty) "" else "+"
	    
	  (coeffstr + addsign + conststr).replace("+-", "-")
	}
	
}
