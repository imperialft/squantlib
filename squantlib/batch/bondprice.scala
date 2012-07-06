
//val valuedate = new org.jquantlib.time.Date(8, 5, 2012)
//val paramset = "20120508-000"

try {
	"Paramset : "  + paramset
}
catch { case e => {
  println("You must define the following paramter")
  println("paramset : String")}
}



/**
 * Creates factory from given paramset.
 * Define following parameters in advance.
 *  val valuedate:Date => market value date
 *  val paramset:String => parameter id
 */

import squantlib.database._
import squantlib.database.schemadefinitions._
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._

val t1 = System.nanoTime
val params:InputParameterSet = new InputParameterSet(transaction { from(DB.inputparameters)(c => where(c.paramset === paramset) select(c)).toSet })
val cdsparams:CDSParameterSet = new CDSParameterSet(transaction { from(DB.cdsparameters)(c => where(c.paramset === paramset) select(c)).toSet })
val dbbonds:Set[Bond] = transaction { from(DB.bonds)(c => select(c)).toSet }
val valuedate = new org.jquantlib.time.Date(params.inputparameters.head.paramdate)

val t2 = System.nanoTime
val ratecurves = params.toLiborDiscountCurves(paramset)
val fxcurves = params.toFXDiscountCurves(paramset)
val cdscurves = cdsparams.toCDSCurves(paramset)
val factory = new DiscountCurveFactory(ratecurves ++ fxcurves, cdscurves)
factory.describe

val t3 = System.nanoTime
val fixedratebonds = dbbonds.map(b => b.toFixedRateBond).filter(b => b != null)

val t4 = System.nanoTime

fixedratebonds.foreach(b => b.setPricingEngine(factory.getdiscountbondengine(b), valuedate))
var errormsg:Map[String, String] = Map.empty
val priceoutput:Map[String, Double] = fixedratebonds.map(b => (b.bondid, try {b.dirtyPrice} catch {case e => {errormsg += (b.bondid -> e.toString); Double.NaN}})).toMap;
    
val pricelist = priceoutput.filter(p => !p._2.isNaN)
val errorlist:Map[String, String] = priceoutput.filter(p => p._2.isNaN).keySet.map(k => (k, (if (errormsg.keySet.contains(k)) errormsg(k) else null))).toMap

val t5 = System.nanoTime

println("\n*** Input ***")
println("valuedate :\t" + valuedate.shortDate)
println("paramset :\t" + paramset)

println("\n*** Created Variables ***")
println("dbbonds => all bonds")
println("factory => discount curve factory")
println("fixedratebonds => id ->  fixed rate bonds (SB or Stepup)")
println("pricelist => bondid -> bond price")
println("errorlist => bondid -> error message")

println("\n*** Result ***")
println(dbbonds.size + " bonds")
println("\tPriced\tError")
println("Fixed:\t" + pricelist.size + "\t" + errorlist.size)
println("\nTotal process time:\t" + "%.3f sec".format((t5 - t1)/1000000000.0))
println("  Fetch from db:\t" + "%.3f sec".format((t2 - t1)/1000000000.0))
println("  Factory construction:\t" + "%.3f sec".format((t3 - t2)/1000000000.0))
println("  Bond construction:\t" + "%.3f sec".format((t4 - t3)/1000000000.0))
println("  Bond Pricing:\t" + "%.3f sec".format((t5 - t4)/1000000000.0))
println("\n*** System Output ***")
