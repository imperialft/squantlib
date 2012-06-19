package squantlib.parameter.yieldparameter

import org.jquantlib.time.{ Date => JDate }

/**
 * Basic Framework for Long-Double Interpolation
 * Points are interpolated between max and min range, and extrapolated outside.
 */
trait AbstractYieldParameter{
	var valuedate : JDate
	val mindays : Long
	val maxdays : Long

	def lowextrapolation(v : Long) : Double
    def highextrapolation(v : Long) : Double
    def interpolation(v : Long) : Double
  
    def value(v : Long) : Double = {
      v match {
        case vv if vv <= mindays => lowextrapolation(vv)
        case vv if vv >= maxdays => highextrapolation(vv)
        case _ => interpolation(v)
          }
    }
}
