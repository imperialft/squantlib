package squantlib.setting.initializer

import org.jquantlib.daycounters._

object Daycounters extends Initializer[DayCounter] {
  
	val mapper = Map(
			("30/360" -> new Thirty360),
			("ACT/360" -> new Actual360),
			("ACT/365" -> new Actual365Fixed),
			("ABSOLUTE" -> new Absolute),
			("ACT/ACT" -> new ActualActual))

}

