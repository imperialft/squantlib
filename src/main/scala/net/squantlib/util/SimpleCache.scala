package net.squantlib.util

import scala.collection.mutable.{WeakHashMap, SynchronizedMap}

class SimpleCache {
  
  val cache = new WeakHashMap[String, Any] with SynchronizedMap[String, Any]
  
  def get[A](name:String):Option[A]= cache.get(name).collect{case v => v.asInstanceOf[A]}
  
  def getAny(name:String):Option[Any]= cache.get(name)
  
  def getOrUpdate[A](name:String, retriever: => A):A = 
    if (cache.contains(name)) cache(name).asInstanceOf[A]
    else {
      val result = retriever
      cache(name) = result
      result
    }
  
  def clear:Unit = cache.clear
  
  def update[A](name:String, retriever: => A):Unit = cache.update(name, retriever)
  
}

class TypedCache[A] {
  
  val cache = new WeakHashMap[String, A] with SynchronizedMap[String, A]
  
  def get(name:String):Option[A]= cache.get(name)
  
  def getOrUpdate(name:String, retriever: => A):A = cache.getOrElseUpdate(name, retriever)
  
  def clear:Unit = cache.clear
  
}
