package squantlib.task.pricing

import squantlib.database._
import squantlib.database.QLConstructors._
import squantlib.database.schemadefinitions.{ Bond => dbBond, _}
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.bonds.FixedRateBond
import squantlib.database.utilities._
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.currencies.Asia.JPYCurrency
import scala.collection.immutable.StringLike

object BondPrice {
  
  private var pendingprice = scala.collection.mutable.ListBuffer.empty[BondPrice]
  var dbbonds:Map[String, dbBond] = Map.empty
  
  def loadbonds = {
    dbbonds = DB.getAllBonds.map(b => (b.id, b)).toMap
  }
  
  def store(prices:List[BondPrice]) = pendingprice.synchronized{
    pendingprice ++= prices
  }
  
  def storedprice = pendingprice
  
  def push = {
    if (pendingprice.size != 0) {
		val targetprices = pendingprice.filter(p => (!p.pricedirty.isNaN))
	    printf("Writing " + targetprices.size + " items to Database...")
		val t1 = System.nanoTime
		DB.setBondPrice(targetprices)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		pendingprice.clear
		}
	}
  
  def price(paramset:String):Unit = {
    
    var outputstring = ""
    def output(s:String):Unit = { outputstring += s }
    def outputln(s:String):Unit = { outputstring += s + "\n"}
    
	outputln("\n*** START OUTPUT " + paramset + " ***")
	
	/**
	 * Creates factory from given paramset.
	 */
	val t1 = System.nanoTime
	val factory = QLDB.getDiscountCurveFactory(paramset)
	val valuedate = factory.valuedate
	val jpyccy = new JPYCurrency
	
	/**
	 * Initialise priceable bonds with default bond engine
	 */
	val t2 = System.nanoTime
	val bonds:List[QLBond] = QLDB.getBonds(factory)
	
	/**
	 * Compute bond price
	 */
	val t3 = System.nanoTime
	val bondprices = bonds map { b => b.bondprice(valuedate, factory) }
	
	
	/**
	 * Display Process Results
	 */
	val t4 = System.nanoTime
	
//	val dbbonds = DB.getAllBonds.map(b => (b.id, b)).toMap;
	if (dbbonds.isEmpty) loadbonds
	val bondlist = bonds.map(b => (b.bondid, b)).toMap;
	val fixedratebonds:Map[String, FixedRateBond] = bonds.map(bond => bond match { case b:FixedRateBond => (b.bondid, b); case _ => null}).filter(b => b != null).toMap;
	val pricelist = bondprices.filter(p => !p.pricedirty.isNaN).map(p => (p.bondid, p)).toMap;
	val bond_product:Map[String, String] = dbbonds.map(b => (b._1, b._2.productid)).toMap;
	
	val gps = bondprices.filter(p => p.pricedirty.isNaN).map(p => (p.bondid, p.comment)).groupBy(p => p._2 match {
	    case s if s == null => "ERROR"
	    case s if s startsWith "expired" => "EXPIRED"
	    case s if s startsWith "too far from issue" => "NOTISSUED"
	    case _ => "ERROR"
	})
	val bondpricelist = bondprices.map(b => b.bondid)
	val notpriced = dbbonds.filter(b => !bondpricelist.contains(b._1)).map(p => (p._1, null))
	  
	val errorlist = if (gps.keySet.contains("ERROR")) gps("ERROR").toMap else null
	val expirelist = if (gps.keySet.contains("EXPIRED")) gps("EXPIRED").toMap else null
	val nonissuelist = if (gps.keySet.contains("NOTISSUED")) gps("NOTISSUED").toMap else null
	val tmap = if (errorlist == null || errorlist.isEmpty) null else scala.collection.immutable.TreeMap(errorlist.toArray:_*)
	
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
	
	outputln("valuedate :\t" + valuedate.shortDate)
	outputln("paramset :\t" + paramset)
	outputln("\n* Market *")
	output(factory.describe)
	outputln("\n" + dbbonds.size + " bonds")
	outputln("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format("PRODUCT", "PRICED", "ERROR", "EXPIRED", "NOTISSUED", "IGNORED"))
	
	resultsummary.foreach { s => outputln("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format(s._1, s._2, s._3, s._4, s._5, s._6))}
	
	outputln("%-10.10s %-8.8s %-8.8s %-8.8s %-8.8s %-8.8s".format("TOTAL", 
	    resultsummary.map(r => r._2).sum, 
	    resultsummary.map(r => r._3).sum, 
	    resultsummary.map(r => r._4).sum,
	    resultsummary.map(r => r._5).sum,
	    resultsummary.map(r => r._6).sum))
	
	val t5 = System.nanoTime
	
	outputln("")
	outputln("%-27.27s %.3f sec".format("Total process time:", ((t5 - t1)/1000000000.0)))
	outputln("  %-25.25s %.3f sec".format("Factory construction:", ((t2 - t1)/1000000000.0)))
	outputln("  %-25.25s %.3f sec".format("Bond collection:", ((t3 - t2)/1000000000.0)))
	outputln("  %-25.25s %.3f sec".format("Bond pricing:", ((t4 - t3)/1000000000.0)))
	outputln("  %-25.25s %.3f sec".format("Result display:", ((t5 - t4)/1000000000.0)))
	
	outputln("\nERRORS:")
	if (errorlist != null) tmap.foreach(e => outputln(e._1 + "\t" + bond_product(e._1) + "\t" + bondlist(e._1).currency + "\t" + bondlist(e._1).maturityDate.shortDate + "\t" + e._2))	
	outputln("\n*** END OUTPUT " + paramset + "***\n")
	
	printf(outputstring)
	
	store(bondprices)
  }
 
}
