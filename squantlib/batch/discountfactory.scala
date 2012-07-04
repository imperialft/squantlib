val vd = new Date(8, 5, 2012)
val paramset = "20120508-000"

/**
 * Creates factory from given paramset.
 * Define following parameters in advance.
 *  val vd:Date => market value date
 *  val paramset:String => parameter id
 */

import squantlib.database._
import squantlib.database.schemadefinitions._
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._

val t1 = System.nanoTime
val params = transaction { from(DB.inputparameters)(c => where(c.paramset === paramset) select(c)).toSet }

val t2 = System.nanoTime
val ratecurves = LiborDiscountCurveConstructor.getcurves(params)
val rateccycurve = ratecurves.map{case ((k1, k2), v) => (k1, v)} toMap;
val fxcurves = FXDiscountCurveConstructor.getcurves(params)
val fxccycurve = fxcurves.map{case ((k1, k2), v) => (k1, v)} toMap;

val t3 = System.nanoTime
val factory = new DiscountCurveFactory(rateccycurve ++ fxccycurve)
factory.describe

println("fetch from db :" + "%d ms".format((t2 - t1)/1000))
println("construct discount curve :" + "%d ms".format((t3 - t2)/1000))
println("factory construction:" + "%d ms".format((System.nanoTime - t3)/1000))
