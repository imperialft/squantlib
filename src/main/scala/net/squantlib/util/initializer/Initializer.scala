package net.squantlib.util.initializer

trait Initializer[A] {
   
	def apply(id:String):Option[A] = mapper.get(id)
	
	def getOrElse(id:String, defaultvalue:A):A = apply(id).getOrElse(defaultvalue)
	
	def contains(s:String):Boolean = mapper.contains(s)
	
	def keySet:Set[String] = mapper.keySet
	
	val mapper:Map[String, A]
}