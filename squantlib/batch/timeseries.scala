

import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, _}
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.bonds.FixedRateBond
import squantlib.database.utilities._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.currencies.Asia.JPYCurrency
import scala.collection.immutable.TreeMap
import squantlib.model.timeseries.TsAnalysis._

val start = (new Date(1,1,2010)).longDate
val end = (new Date(1,1,2013)).longDate
//val fxset = DB.getFXTimeSeries(start, end, "EUR", "JPY")


