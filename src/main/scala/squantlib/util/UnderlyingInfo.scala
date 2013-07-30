package squantlib.util

import squantlib.database.DB
import squantlib.database.schemadefinitions.Underlying
import scala.collection.mutable.WeakHashMap
import squantlib.util.JsonUtils._
import squantlib.util.DisplayUtils._

object UnderlyingInfo {
  
//	val cache = new WeakHashMap[String, Option[Underlying]]
  
    val cache = new AutoTimedCache(30000)
	
	def apply(id:String):Option[Underlying] = cache.get(id, DB.getUnderlying(id))
	
	def name(id:String):String = apply(id) match {
	  case Some(v) => v.name
	  case None => id
	}
	
	def jsonNames(jsonstring:String):String = jsonstring.jsonArray match {
	  case s if s.isEmpty => null
	  case s => val vmap:Map[String, String] = s.map(p => (p.asText, name(p.asText)))(collection.breakOut)
	  			jsonString(vmap)
	}
	
	
	def nameJpn(id:String):String = apply(id) match {
	  case Some(v) => v.namejpn
	  case None => id
	}
	
	def jsonJpnNames(jsonstring:String):List[String] = jsonstring.parseJsonStringList.map(s => nameJpn(s.orNull))
	
	def displayValue(id:String, d:Double):String = apply(id) match {
	  case Some(v) => v.display(d)
	  case None => d.asDouble
	}
	
	def fixingInfo(textInfo:String, variables:List[String]) = {
	  val varMap = variables.map(v => ("@" + v, "条件決定日の" + nameJpn(v)))
	  var result = textInfo
	  varMap.foreach{case (b, a) => result = result.replace(b, a)}
	  result
	}
	
}


