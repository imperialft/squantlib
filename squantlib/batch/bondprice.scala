
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


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

/**
 * Creates factory from given paramset.
 */
val t1 = System.nanoTime
val factory = QLDB.getDiscountCurveFactory(paramset)
val valuedate = factory.valuedate
val jpyccy = new JPYCurrency

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
 * Compute bond price
 */
val t3 = System.nanoTime
val bondprices = bonds map{ b => {
  val termstructure = factory.getyieldtermstructure(b)
  QLDB.getBondPrice(b, factory.valuedate, factory.fx(b.currency.code, jpyccy.code), paramset, termstructure)
}}


/**
 * Display Process Results
 */
val t4 = System.nanoTime
println("\n*** Input ***")
println("valuedate :\t" + valuedate.shortDate)
println("paramset :\t" + paramset)

println("\n*** Created Variables ***")
println("dbbonds:\t all bonds")
val dbbonds = DB.getAllBonds.map(b => (b.id, b)).toMap;

println("factory:\t discount curve factory")
println("bondlist:\t list of all bonds")
val bondlist = bonds.map(b => (b.bondid, b)).toMap;

println("fixedratebonds:\t list of all fixed rate bonds")
val fixedratebonds:Map[String, FixedRateBond] = bonds.map(bond => bond match { case b:FixedRateBond => (b.bondid, b); case _ => null}).filter(b => b != null).toMap;

println("pricelist:\t list of valid bond price")
val pricelist = bondprices.filter(p => !p.pricedirty.isNaN).map(p => (p.bondid, p)).toMap;

val bond_product:Map[String, String] = dbbonds.map(b => (b._1, b._2.productid)).toMap;

val (errorlist, expirelist, nonissuelist, notpriced) = {
  val gps = bondprices.filter(p => p.pricedirty.isNaN).map(p => (p.bondid, p.comment)).groupBy(p => p._2 match {
    case s if s == null => "ERROR"
    case s if s startsWith "expired" => "EXPIRED"
    case s if s startsWith "too far from issue" => "NOTISSUED"
    case _ => "ERROR"
  })
  val bondpricelist = bondprices.map(b => b.bondid)
  val notpriced = dbbonds.filter(b => !bondpricelist.contains(b._1)).map(p => (p._1, null))
  
  (if (gps.keySet.contains("ERROR")) gps("ERROR").toMap else null, 
   if (gps.keySet.contains("EXPIRED")) gps("EXPIRED").toMap else null, 
   if (gps.keySet.contains("NOTISSUED")) gps("NOTISSUED").toMap else null,
   notpriced)
}


println("\n*** Result ***")
println(dbbonds.size + " bonds")
println("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format("PRODUCT", "PRICED", "ERROR", "EXPIRED", "NOTISSUED", "IGNORED"))

val resultsummary = bond_product.groupBy(p => p._2).map{ p => {
  val vdlong = valuedate.longDate
  val bondids = bond_product.filter(b => b._2 == p._1).map(b => b._1)
  val valids = if (pricelist == null) 0 else bondids.filter(b => pricelist.contains(b)).size
  val errors = if (errorlist == null) 0 else bondids.filter(b => errorlist.keySet.contains(b)).size
  val expires = if (expirelist == null) 0 else bondids.filter(b => expirelist.keySet.contains(b)).size
  val notissueds = if (nonissuelist == null) 0 else bondids.filter(b => nonissuelist.keySet.contains(b)).size
  val notpriceds = if (notpriced == null) 0 else bondids.filter(b => notpriced.keySet.contains(b)).size
  (p._1, valids, errors, expires, notissueds, notpriceds)
}}

resultsummary.foreach { s => {
	println("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format(s._1, s._2, s._3, s._4, s._5, s._6))
}}
println("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format("TOTAL", 
    resultsummary.map(r => r._2).sum, 
    resultsummary.map(r => r._3).sum, 
    resultsummary.map(r => r._4).sum,
    resultsummary.map(r => r._5).sum,
    resultsummary.map(r => r._6).sum))

val t5 = System.nanoTime
println("")
println("%-27.27s %.3f sec".format("Total process time:", ((t5 - t1)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Factory construction:", ((t2 - t1)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Bond collection:", ((t3 - t2)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Bond pricing & db write:", ((t4 - t3)/1000000000.0)))
println("  %-25.25s %.3f sec".format("Result display:", ((t5 - t4)/1000000000.0)))

println("\n*** Action ***")
println("showprice:\t display all prices")
def showprice = bondprices.filter(p => !p.pricedirty.isNaN).foreach(p => println("%-15.15s %-10.10s %-3.3s".format(p.bondid, p.pricedirty, p.currencyid)))

println("showerrors:\t display error messages")
def showerrors = errorlist.foreach(p => println("%-15.15s %-10s".format(p._1, p._2)))

println("showexpired:\t display expired bonds")
def showexpired = (expirelist ++ nonissuelist).foreach(p => println("%-15.15s %-10s".format(p._1, p._2)))

println("pushdb:\t update price to database")
def pushdb = {
	println("Writing to Database")
	val start = System.nanoTime
	DB.setBondPrice(bondprices.filter(p => (!p.pricedirty.isNaN)))
	val end = System.nanoTime
	println("Done (%.3f sec)".format(((end - start)/1000000000.0)))
}

println("\n*** System Output ***")
