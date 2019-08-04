package net.squantlib.util

import com.fasterxml.jackson.databind.JsonNode
import scala.language.implicitConversions
import scala.annotation.tailrec
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging._
//import com.typesafe.scalalogging.slf4j._


object DisplayUtils { // extends StrictLogging {
   
  var defaultNaNdisplay = "[]"

  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

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
      @tailrec def trimAcc(d:String):String = d match {
        case d if d.isEmpty => "0"
        case d if (d takeRight 2) == ".0" => d.dropRight(2)
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
        case '：' => ':'
        case c => c
      }
      result
    }
  }
  
  implicit def doubleToExtendedDouble(d:Double) = ExtendedDouble(d)
  case class ExtendedDouble(d:Double) {
    def asPercent:String = (if (d.isNaN || d.isInfinity) defaultNaNdisplay else "%.4f".format(d * 100.0).trimZeros) + "%"
    def asPercent(decimals:Int):String = (if (d.isNaN || d.isInfinity) defaultNaNdisplay else ("%." + decimals + "f").format(d * 100.0).trimZeros) + "%"
    
    def asDouble:String = if (d.isNaN || d.isInfinity) defaultNaNdisplay else "%,.4f".format(d).trimZeros
    def asDouble(decimals:Int):String = if (d.isNaN || d.isInfinity) defaultNaNdisplay else ("%,." + decimals + "f").format(d).trimZeros

    def getDecimal(precision:Int, roundType:String):Option[BigDecimal] = ExtendedDouble.getDecimal(d, precision, roundType)
    def getDecimal(underlyingId:String)(implicit fixingInfo:FixingInformation):Option[BigDecimal] = ExtendedDouble.getDecimal(d, underlyingId)

    def getRoundedDouble(precision:Int, roundType:String):Double = getDecimal(precision, roundType).collect{case v => v.toDouble}.getOrElse(Double.NaN)
    def getRoundedDouble(underlyingId:String)(implicit fixingInfo:FixingInformation):Double = {
      getDecimal(underlyingId).collect{case v => v.toDouble}.getOrElse(Double.NaN)
    }

    def compareWithNaN(a:Option[BigDecimal], b:Option[BigDecimal], op:(BigDecimal, BigDecimal) => Boolean):Boolean = {
      (a, b) match {
        case (Some(av), Some(bv)) => op(av, bv)
        case _ => false
      }
    }

    def compareWithNaN(a:Option[BigDecimal], b:BigDecimal, op:(BigDecimal, BigDecimal) => Boolean):Boolean = {
      a match {
        case Some(av) => op(av, b)
        case _ => false
      }
    }

    def ~<(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(getDecimal(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a < b)
    def ~<(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a < b)
    def ~<(other: BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDecimal.scaled(other, underlyingId), (a, b) => a < b)

    def ~<=(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(getDecimal(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a <= b)
    def ~<=(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a <= b)
    def ~<=(other: BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDecimal.scaled(other, underlyingId), (a, b) => a <= b)

    def ~==(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(getDecimal(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a == b)
    def ~==(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a == b)
    def ~==(other: BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDecimal.scaled(other, underlyingId), (a, b) => a == b)

    def ~>(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(getDecimal(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a > b)
    def ~>(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a > b)
    def ~>(other: BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDecimal.scaled(other, underlyingId), (a, b) => a > b)

    def ~>=(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(getDecimal(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a >= b)
    def ~>=(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a >= b)
    def ~>=(other: BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(ExtendedDouble.getDecimal(d, underlyingId), ExtendedDecimal.scaled(other, underlyingId), (a, b) => a >= b)

  }

  object ExtendedDouble {
    def getDecimal(v:Double, precision:Int, roundType:String):Option[BigDecimal] = {
      if (v.isInfinite || v.isNaN) None
      else roundType match {
        case "roundup" => Some(BigDecimal.valueOf(v).setScale(precision, BigDecimal.RoundingMode.CEILING))
        case "rounddown" => Some(BigDecimal.valueOf(v).setScale(precision, BigDecimal.RoundingMode.FLOOR))
        case _ => Some(BigDecimal.valueOf(v).setScale(precision, BigDecimal.RoundingMode.HALF_UP))
      }
    }

    def getDecimal(v:Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Option[BigDecimal] = getDecimal(v, fixingInfo.getUnderlyingPrecision(underlyingId), fixingInfo.getUnderlyingRoundType(underlyingId))

  }


  implicit def decimalToExtendedDecimal(d:BigDecimal) = ExtendedDecimal(d)

  case class ExtendedDecimal(d:BigDecimal) {
    def asPercent:String = "%.4f".format(d * 100.0).trimZeros + "%"
    def asPercent(decimals:Int):String = ("%." + decimals + "f").format(d * 100.0).trimZeros + "%"

    def asDouble:String = "%,.4f".format(d).trimZeros
    def asDouble(decimals:Int):String = ("%,." + decimals + "f").format(d).trimZeros

    def scaled(precision:Int, roundType:String):BigDecimal = ExtendedDecimal.scaled(d, precision, roundType)
    def scaled(underlyingId:String)(implicit fixingInfo:FixingInformation):BigDecimal = scaled(fixingInfo.getUnderlyingPrecision(underlyingId), fixingInfo.getUnderlyingRoundType(underlyingId))

    def compareWithNaN(a:BigDecimal, b:Option[BigDecimal], op:(BigDecimal, BigDecimal) => Boolean):Boolean = {
      b match {
        case Some(bv) => op(a, bv)
        case _ => false
      }
    }

    def ~<(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(scaled(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a < b)
    def ~<(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(scaled(underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a < b)

    def ~<=(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(scaled(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a <= b)
    def ~<=(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(scaled(underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a <= b)

    def ~==(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(scaled(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a == b)
    def ~==(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(scaled(underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a == b)

    def ~>(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(scaled(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a > b)
    def ~>(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(scaled(underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a > b)

    def ~>=(other: Double, precision:Int, roundType:String):Boolean = compareWithNaN(scaled(precision, roundType), ExtendedDouble.getDecimal(other, precision, roundType), (a, b) => a >= b)
    def ~>=(other: Double, underlyingId:String)(implicit fixingInfo:FixingInformation):Boolean = compareWithNaN(scaled(underlyingId), ExtendedDouble.getDecimal(other, underlyingId), (a, b) => a >= b)

  }

  object ExtendedDecimal {
    def scaled(v:BigDecimal, precision:Int, roundType:String):BigDecimal = roundType match {
      case "roundup" => v.setScale(precision, BigDecimal.RoundingMode.CEILING)
      case "rounddown" => v.setScale(precision, BigDecimal.RoundingMode.FLOOR)
      case _ => v.setScale(precision, BigDecimal.RoundingMode.HALF_UP)
    }

    def scaled(v:BigDecimal, underlyingId:String)(implicit fixingInfo:FixingInformation):BigDecimal = scaled(v, fixingInfo.getUnderlyingPrecision(underlyingId), fixingInfo.getUnderlyingRoundType(underlyingId))

  }

  implicit def extendUnderlyingPrices(ulv: Map[String, Double]) = ExtendedUnderlyingPrices(ulv)

  case class ExtendedUnderlyingPrices(ulv:Map[String, Double]) {
    def getDecimal()(implicit fixingInfo: FixingInformation):Map[String, BigDecimal] = ulv.map { case (ul, v) => (ul, v.getDecimal(ul))}.collect{case (ul, Some(v)) => (ul, v)}
  }

  implicit def extendUnderlyingPricesDecimal(ulv: Map[String, BigDecimal]) = ExtendedUnderlyingPricesDecimal(ulv)

  case class ExtendedUnderlyingPricesDecimal(ulv:Map[String, BigDecimal]) {
    def getDouble = ulv.map { case (ul, v) => (ul, v.toDouble) }
  }

  implicit def doubleToExtendedDoubleOpt(d:Option[Double]) = ExtendedDoubleOpt(d)
  case class ExtendedDoubleOpt(d:Option[Double]) {
    
    def asPercentOr(alternative:String, prefix:String, suffix:String):String = d match {
      case Some(r) if !r.isNaN && !r.isInfinity => prefix + "%.4f".format(r * 100.0).trimZeros + "%" + suffix
      case None => alternative
    }
    def asPercentOr(alternative:String):String = asPercentOr(alternative, "", "")
    def asPercent = asPercentOr("")
    
    def asDoubleOr(alternative:String, prefix:String, suffix:String):String = d match {
      case Some(r) if !r.isNaN && !r.isInfinity => prefix + "%,.4f".format(r).trimZeros + suffix
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
  
  def measuredProcess[T](id:String, processName:String, showStart:Boolean = true, result:T => String = (x:T) => "")(p: => T):T = {
    if (showStart) logger.info(s"${id} : Start ${processName}")
    val t1 = System.currentTimeMillis
    val x = p
    val t2 = System.currentTimeMillis
    val r = result(x)
    logger.info(s"""${id} : Done ${processName} ${if (r == null || r.isEmpty) "" else s"- ${r}"} (${"%.2fs".format(((t2 - t1) / 1000.0))})""")
    x
  }
  
  def standardOutput(s:Any*) =
    if (s.size == 1) logger.info(s.head.toString)
    else logger.info((s.head.toString :: " : " :: s.tail.map(_.toString).toList).mkString(" "))

  def errorOutput(s:Any*) =
    if (s.size == 1) logger.error(s.head.toString)
    else logger.error((s.head.toString :: " : " :: s.tail.map(_.toString).toList).mkString(" "))

}
