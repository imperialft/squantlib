package net.squantlib.database

import net.squantlib.util.JsonUtils._
import net.squantlib.util.DisplayUtils._
import scala.collection.JavaConversions._
import net.squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode

package object schemadefinitions {
  
  def getObjectFieldMap[T <: AnyRef](obj:T): Map[String, Any] = {
    val fieldsAsPairs = for (field <- obj.getClass.getDeclaredFields) yield {
      field.setAccessible(true)
      (field.getName, field.get(obj))
    }
    Map(fieldsAsPairs :_*)
  }
  
  protected def emptyStringToNull(s:Any) = s match {
    case x:String if x == null || x.trim.isEmpty => null
    case x => x
  }
  
  def compareValues(a:Any, b:Any) = emptyStringToNull(a) == emptyStringToNull(b)
  
  def compareMap(m1:Map[String, Any], m2:Map[String, Any], ignoredFields:Set[String] = Set.empty):Boolean = m1.forall{
    case (k, _) if ignoredFields contains k => true
    case (k, _) if k.head == '_' => true
    case (k, v) => m2.get(k) match {
      case Some(vv) if (compareValues(v, vv)) => true 
      case _ => standardOutput(k, "field changed"); false
    }
  }

  def compareObjects[T <: AnyRef](a:T, b:T, ignoredFields:Set[String]):Boolean = {
    compareMap(getObjectFieldMap(a), getObjectFieldMap(b), ignoredFields)
  }
  
  def stringList(s:String, ifNone:String = null):List[String] = s.parseJsonStringList.map(_.getOrElse(ifNone))
  
  def booleanList(s:String, compareTo:String):List[Boolean] = s.parseJsonStringList.map(_.orNull == compareTo)
  
  def multipleReplace(v:String, replacements:Map[String, Any]):String = {
    if (v == null) {return null}
    var result = v
    replacements.foreach{case (k, d) => result = result.replace(k, d.toString)}
    result
  }

}