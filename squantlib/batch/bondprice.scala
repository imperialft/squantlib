
/**
 * Creates factory from given paramset.
 * Define following parameters in advance.
 *  val paramset:String => parameter id
 */
val paramset = "20120508-000"

try {
	"Paramset : "  + paramset
}
catch { case e => {
  println("You must define the following paramter")
  println("paramset : String")}
}


import squantlib.database._
import squantlib.database.schemadefinitions.{ Bond => dbBond, _}
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.bonds.FixedRateBond
import squantlib.database.utilities._
import org.jquantlib.instruments.{Bond => QLBond}

/**
 * Creates factory from given paramset.
 */
val t1 = System.nanoTime
val factory = QLDB.getDiscountCurveFactory(paramset)
val valuedate = factory.valuedate


/**
 * Initialise bonds with bond engine
 */
val t2 = System.nanoTime
val bonds:List[QLBond] = {
  val fixedrateproducts = List("SB", "STEPUP", "DISC")
  val fixedrateids = DB.getBondsByProducts(fixedrateproducts)
  val fixedratebuilder = (b:dbBond) => b.toFixedRateBond
  val fixedrateengine = (b:QLBond) => factory.getdiscountbondengine(b)
  
  val fixedratebonds = QLDB.getBonds(fixedrateids, fixedratebuilder, fixedrateengine, valuedate)
  
  fixedratebonds
}

/**
 * Compute bond price and output to database
 */
val t3 = System.nanoTime
val bondprices = QLDB.setBondPrice(bonds, factory, true)


/**
 * Display Process Results
 */
val t4 = System.nanoTime
println("\n*** Input ***")
println("valuedate :\t" + valuedate.shortDate)
println("paramset :\t" + paramset)

println("\n*** Created Variables ***")
println("dbbonds => all bonds")
val dbbonds = DB.getAllBonds.map(b => (b.id, b)).toMap;

println("factory => discount curve factory")
println("bonds => list of all bonds")

println("fixedratebonds => list of all fixed rate bonds")
val fixedratebonds:Map[String, FixedRateBond] = bonds.map(bond => bond match { case b:FixedRateBond => (b.bondid, b); case _ => null}).filter(b => b != null).toMap;

println("bondprices => list of bond price (valid and non-valid prices)")

println("errorlist => bondid -> error message")
val errorlist = bondprices.filter(p => p.pricedirty.isNaN).map(p => (p.bondid, p.comment)).toMap;

println("\n*** Result ***")
println(dbbonds.size + " bonds")
println("%-10.10s %-8.8s %-8.8s %-8.8s".format("PRODUCT", "PRICED", "ERROR", "EXPIRED"))
val bond_product:Map[String, String] = dbbonds.map(b => (b._1, b._2.productid)).toMap;
val resultsummary = bond_product.groupBy(p => p._2).map{ p => {
  val vdlong = valuedate.longDate
  val validprices = bondprices.filter(c => !c.pricedirty.isNaN).filter(c => p._2.contains(c.bondid)).size
  val expired = bond_product.filter(b => b._2 == p._1).filter(b => dbbonds(b._1).maturity.compareTo(vdlong) <= 0).size
  val invalidprices = p._2.size - validprices - expired
  (p._1, validprices, invalidprices, expired)
}}

resultsummary.foreach { s => {
	println("%-10.10s %-8.8s %-8.8s %-8.8s".format(s._1, s._2, s._3, s._4))
}}
println("%-10.10s %-8.8s %-8.8s %-8.8s".format("TOTAL", resultsummary.map(r => r._2).sum, resultsummary.map(r => r._3).sum, resultsummary.map(r => r._4).sum))

val t5 = System.nanoTime
println("")
println("%-27.27s %.3f sec".format("Total process time:", ((t5 - t1)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Factory construction:", ((t2 - t1)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Bond collection:", ((t3 - t2)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Bond pricing & db write:", ((t4 - t3)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Result display:", ((t5 - t4)/1000000000.0)))
println("\n*** System Output ***")

