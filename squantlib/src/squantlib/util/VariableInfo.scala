package squantlib.util

import squantlib.database.DB
import squantlib.database.schemadefinitions.Underlying
import scala.collection.mutable.WeakHashMap

object VariableInfo {
  
	val cache = new WeakHashMap[String, Option[Underlying]]
	
	def apply(id:String):Option[Underlying] = cache.getOrElseUpdate(id, DB.getUnderlying(id))
	
	def name(id:String):String = apply(id) match {
	  case Some(v) => v.name
	  case None => id
	}
	
	def namejpn(id:String):String = apply(id) match {
	  case Some(v) => v.namejpn
	  case None => id
	}
	
	def displayValue(id:String, d:Double):String = apply(id) match {
	  case Some(v) => v.display(d)
	  case None => id
	}
	
	def fixingInfo(textInfo:String, variables:List[String]) = {
	  val varMap = variables.map(v => ("@" + v, "条件決定日の" + namejpn(v)))
	  var result = textInfo
	  varMap.foreach{case (b, a) => result = result.replace(b, a)}
	  result
	}
	
}


