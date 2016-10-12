package net.squantlib.util.initializer

import org.jquantlib.daycounters._

object Daycounters extends Initializer[DayCounter] {
  
	val mapper = Map(
			("30/360" -> new Thirty360),
			("ACT/360" -> new Actual360),
			("ACT/365" -> new Actual365Fixed),
			("ABSOLUTE" -> new Absolute),
			("ACT/ACT" -> new ActualActual),
			("30/360 (Bond Basis)" -> new Thirty360),
			("Actual/360" -> new Actual360),
			("Actual/365 (fixed)" -> new Actual365Fixed),
			("ABSOLUTE" -> new Absolute),
			("Actual/Actual (ISDA)" -> new ActualActual))

}

