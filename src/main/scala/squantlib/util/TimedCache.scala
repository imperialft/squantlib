package squantlib.util

import scala.collection.mutable.WeakHashMap

class TimedCache {
  
  private def currenttime = java.util.Calendar.getInstance.getTime.getTime
  var cached = false
  private var endtime = currenttime
  
  def startCache(timeout:Long) = {
    cached = true
    endtime = currenttime + timeout
  }
  
  def endCache = {
    cache.clear
    cached = false
  }

  val cache = WeakHashMap.empty[String, AnyRef]
  
  def get[A <: AnyRef](name:String, retriever: => A):A = {
    if (cached && currenttime > endtime) endCache
    val result = if (cache.contains(name)) cache(name).asInstanceOf[A] else retriever
    if (cached) cache(name) = result
    result
  }  
}

class AutoTimedCache(timeout:Long) extends TimedCache {
  
  override def get[A <: AnyRef](name:String, retriever: => A):A = {
    if (cached == false) startCache(timeout)
    super.get(name, retriever)
  }
  
}
