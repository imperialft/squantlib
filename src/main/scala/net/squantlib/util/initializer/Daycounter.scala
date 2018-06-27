package net.squantlib.util.initializer

import org.jquantlib.daycounters._
import org.jquantlib.time._

object Daycounters extends Initializer[DayCounter] {
  
	val mapper = Map(
			("ABSOLUTE" -> new Absolute),

			("30/360" -> new Thirty360),
			("30/360 (Bond Basis)" -> new Thirty360),

			("ACT/360" -> new Actual360),
			("Actual/360" -> new Actual360),

			("ACT/365" -> new Actual365Fixed),
			("Actual/365 (fixed)" -> new Actual365Fixed),


			("ACT/ACT" -> new ActualActual),
			("Actual/Actual (ISDA)" -> new ActualActual),

			("ACT/ACT12M" -> new ActualActual(ActualActual.Convention.ISMA, new Period(12, TimeUnit.Months))),
			("Actual/Actual (ICMA 12M)" -> new ActualActual(ActualActual.Convention.ISMA, new Period(12, TimeUnit.Months))),
			("ACT/ACT6M" -> new ActualActual(ActualActual.Convention.ISMA, new Period(6, TimeUnit.Months))),
			("Actual/Actual (ICMA 6M)" -> new ActualActual(ActualActual.Convention.ISMA, new Period(6, TimeUnit.Months))),
			("ACT/ACT3M" -> new ActualActual(ActualActual.Convention.ISMA, new Period(3, TimeUnit.Months))),
			("Actual/Actual (ICMA 3M)" -> new ActualActual(ActualActual.Convention.ISMA, new Period(3, TimeUnit.Months))),
			("ACT/ACT1M" -> new ActualActual(ActualActual.Convention.ISMA, new Period(1, TimeUnit.Months))),
			("Actual/Actual (ICMA 1M)" -> new ActualActual(ActualActual.Convention.ISMA, new Period(1, TimeUnit.Months)))
	)

}

