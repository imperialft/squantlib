package squantlib.util


case class FixingInformation(current:Option[Double], minRange:Option[Double], maxRange:Option[Double], initialFixing:Map[String, Double]) {
    
  def all:Map[String, Double] = current match {
    case Some(c) => initialFixing.updated("tbd", c)
    case None => initialFixing
  }
    
  def update(p:String):String = multipleReplace(p, all.map{case (k, v) => ("@" + k, v)})
  
  private def multipleReplace(v:String, replacements:Map[String, Any]):String = {
	if (v == null) {return null}
	var result = v
	replacements.foreach{case (k, d) => result = result.replace(k, d.toString)}
	result
  }
    
}
  
object FixingInformation {
  
  def empty = FixingInformation(None, None, None, Map.empty)
  
}
