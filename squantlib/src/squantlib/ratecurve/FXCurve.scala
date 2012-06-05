package squantlib.ratecurve

import squantlib.parameter.TimeVector


/**
 * Encapsulates a full FX curve. Should implement getZC() in superclass.
 */
trait FXCurve extends DiscountableCurve{
  val fx : Double
  val fwdfx : TimeVector
}
