package squantlib.task.pricing

import squantlib.database._
import squantlib.database.QLConstructors._
import squantlib.database.schemadefinitions.{ Bond => dbBond, _}
import squantlib.database.objectconstructor._
import squantlib.model.discountcurve._
import squantlib.instruments.bonds.{JGBFloatBond, JGBFixedBond}
import squantlib.setting.PricingConvention.{bondPriceFrom, priceFrom}
//import squantlib.setting.PricingConvention
import org.jquantlib.time._
import org.squeryl.PrimitiveTypeMode._
import org.jquantlib.instruments.bonds.FixedRateBond
import org.jquantlib.instruments.{Bond => QLBond}
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.time.{Date => qlDate}
import scala.collection.immutable.StringLike
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import scala.collection.immutable.TreeMap
import java.util.{Date => JavaDate}

object BondPrices {
  
  var storedprice = new HashSet[BondPrice] with SynchronizedSet[BondPrice]
  var counter:Int = 0
  
  def setcount(i:Int) = synchronized { counter = i }
  def addcount = synchronized { counter = counter + 1 }
  
  var dbbonds:Map[String, dbBond] = Map.empty
  
  def loadbonds:Unit = {
    dbbonds = DB.getBonds.map(b => (b.id, b)).toMap
  }
  
  def push:Unit = { 
    if (storedprice.size != 0) {
    	printf("Extracting valid price ..")
		storedprice.retain(!_.pricedirty.isNaN)
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(storedprice, false)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		storedprice.clear
		}
	}
   
  def notPricedBonds():Set[String] = 
    DB.getLatestBondPriceParam match {
      case None => Set.empty // Empty bond price
      case Some((pset, pdate)) => notPricedBonds(pset) 
  }
  
  def notPricedBonds(paramset:String):Set[String] = {
	val factory:DiscountCurveFactory = QLDB.getDiscountCurveFactory(paramset).orNull
	val priceablebonds = QLDB.getBonds(factory).filter(b => {
	  bondPriceFrom(b) match {
	  case None => b.isPriceable
	  case Some(d) => b.isPriceable && factory.valuedate.ge(d)
	}})
	
	val pricedbonds = DB.getPricedBondIDs(paramset = paramset)
	priceablebonds.filter(b => !pricedbonds.contains(b.bondid)).map(_.bondid)
  }
  
  def notPricedParams:Set[(String, JavaDate)] = DB.getLatestBondPriceParam match {
    case None => DB.getParamSets
    case Some(p) => DB.getParamSetsAfter(p._2)
  }
  
  def notPricedParams(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    DB.getParamSets.filter(d => !d._2.after(toDate) && !d._2.before(fromDate))

  def updateNewDates:Unit = updateNewDates(false)
    
  def updateNewDates(par:Boolean):Unit = 
    notPricedParams match {
      case params if params.isEmpty => {}
      case params => if (par) params.par.foreach(p => quickprice(p._1))
    		  		else params.foreach(p => quickprice(p._1))
    }
    
  def updateNewDates(fromDate:JavaDate, toDate:JavaDate, par:Boolean = false):Unit = {
    val params = notPricedParams(fromDate, toDate)
    val bonds = notPricedBonds(params.minBy(_._2)._1) | notPricedBonds(params.maxBy(_._2)._1)
    
    notPricedParams(fromDate, toDate) match {
      case params if params.isEmpty => {}
      case params => if (par) params.par.foreach(p => quickprice(p._1, bonds))
    		  		else params.foreach(p => quickprice(p._1, bonds))
    }
  }
  
  def updateNewDates_noJGB(fromDate:JavaDate, toDate:JavaDate, par:Boolean = false):Unit = {
    val params = notPricedParams(fromDate, toDate)
    val startbonds = notPricedBonds(params.minBy(_._2)._1).filter(b => !b.contains("JGB"))
    val endbonds = notPricedBonds(params.maxBy(_._2)._1).filter(b => !b.contains("JGB"))
    val bonds = startbonds | endbonds
    println("#bonds: start:" + startbonds.size + " end:" + endbonds.size + " compute:" + bonds.size + " bonds")
    
    notPricedParams(fromDate, toDate) match {
      case params if params.isEmpty => {}
      case params => if (par) params.par.foreach(p => quickprice(p._1, bonds))
    		  		else params.foreach(p => quickprice(p._1, bonds))
    }
  }
  
  def updateNewBonds(par:Boolean = false):Unit = {
    if (notPricedBonds.isEmpty) return
    val bonds = DB.getBonds(notPricedBonds)
    val startdates = bonds.map(priceFrom).flatMap(s => s)
    if (startdates.isEmpty) return
	val dates = DB.getParamSetsAfter(startdates.min)
	val notpriced = notPricedBonds
	setcount(0)
	if (!dates.isEmpty) {
		if (par) dates.par.foreach(d => quickprice(d._1, notpriced))
		else dates.foreach(d => quickprice(d._1, notpriced))
	}
  }
  
  def quickprice(paramset:String):Unit = quickprice(paramset, null)
  
  def quickprice(paramset:String, bondid:Set[String]):Unit = {
	val factory = QLDB.getDiscountCurveFactory(paramset).orNull
	if (factory == null || factory.curves.size == 0) {
	  println("Factory not found - " + paramset) 
	  return
	}
	
	val bonds:Set[QLBond] = if (bondid == null) QLDB.getBonds(factory) 
							else QLDB.getBonds(bondid, factory)
	
	val bondprices = bonds.map{ b => {
	 b.bondprice(factory) match {
	   case p if p.pricedirty.isNaN => null
	   case p => {println(p); p}
	   }
	 }
	}.filter(_ != null)
	
	storedprice ++= bondprices
	addcount
  }
  
  
  def price(paramsets:Set[String]):Unit = price(paramsets, null)
  
  def price(paramsets:Set[String], bondid:Set[String]):Unit ={
    val params:Set[String] = paramsets.toSet
    params.foreach(d => price(d, bondid))
  } 
  
  def price(paramset:String):Unit = price(paramset, null.asInstanceOf[Set[String]])
    
  def price(paramset:String, bondid:Set[String]):Unit = {
    var outputstring = ""
    def output(s:String):Unit = { outputstring += s }
    def outputln(s:String):Unit = { outputstring += s + "\n"}
    
	outputln("\n*** START OUTPUT " + paramset + "(" + counter + ") ***")
	 
	/**
	 * Creates factory from given paramset.
	 */
	val t1 = System.nanoTime
	val factory = QLDB.getDiscountCurveFactory(paramset).orNull
	if (factory == null || factory.curves.size == 0) {
	  outputln("Curve not found") 
	  return
	}
	  
	val valuedate = factory.valuedate
	val jpyccy = new JPYCurrency
	
	/**
	 * Initialise priceable bonds with default bond engine
	 */
	val t2 = System.nanoTime 
	val bonds:Set[QLBond] = if (bondid == null) QLDB.getBonds(factory) else QLDB.getBonds(bondid, factory)
	
	/**
	 * Compute bond price
	 */
	val t3 = System.nanoTime
	val bondprices = bonds map (_.bondprice(factory))
	
	
	/**
	 * Display Process Results
	 */
	val t4 = System.nanoTime
	
	if (dbbonds.isEmpty) loadbonds
	val bondlist = bonds.map(b => (b.bondid, b)).toMap;
//	val fixedratebonds:Map[String, FixedRateBond] = bonds.collect { case b:FixedRateBond => (b.bondid, b)}.toMap;
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
	val tmap:TreeMap[String, String] = if (errorlist == null || errorlist.isEmpty) null else TreeMap(errorlist.toArray:_*)
	
	val resultsummary = bond_product.groupBy(_._2).map{ case (product, bondproduct) => {
	  val vdlong = valuedate.longDate
	  val bondids = bond_product.filter(_._2 == product).map(_._1)
	  val valids = if (pricelist == null) 0 else bondids.filter(pricelist.contains).size
	  val errors = if (errorlist == null) 0 else bondids.filter(errorlist.keySet.contains).size
	  val expires = if (expirelist == null) 0 else bondids.filter(expirelist.keySet.contains).size
	  val notissueds = if (nonissuelist == null) 0 else bondids.filter(nonissuelist.keySet.contains).size
	  val notpriceds = if (notpriced == null) 0 else bondids.filter(notpriced.keySet.contains).size
	  (product, valids, errors, expires, notissueds, notpriceds)
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
	storedprice ++= bondprices.filter(p => !p.pricedirty.isNaN)
	addcount
  }

//-------------------------------------------------------  
  
  def push(bondprices:Set[BondPrice]):Unit = { 
    if (bondprices.size != 0) {
    	printf("Extracting valid price ..")
	    printf("Writing " + bondprices.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(bondprices.filter(!_.pricedirty.isNaN), false)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		}
	}
  
  def updateJGBR(par:Boolean = false):Unit = {
    println("searching not priced bonds")
	val notpriced = notPricedBonds.filter(_.contains("JGBR"))
    if (notpriced.isEmpty) return
    notpriced.foreach(println)
	counter = 0
	val bondcount = notpriced.size
	notpriced.par.foreach { b => {
	  println("start " + b + " (" + counter + " / " + bondcount + ")")
	  priceNoFactory(b)
		}
    }
  }
  
  def JGBRConstructor(dbbond:dbBond, valuedate:qlDate):Option[QLBond] = {
		dbbond match {
		    case p if JGBRFixedBond.isCompatible(p) => JGBRFixedBond(p, valuedate)
		    case p if JGBRFloatBond.isCompatible(p) => JGBRFloatBond(p, valuedate)
		    case _ => None
		  }
	}
  
  def setJGBRPricingEngine(bond:QLBond, valuedate:qlDate):Unit = {
		bond match {
		    case p:JGBFixedBond => JGBRFixedBond.setDefaultPricingEngine(p, valuedate)
		    case p:JGBFloatBond => JGBRFloatBond.setDefaultPricingEngine(p, valuedate)
		    case _ => {}
		  }
	}
  
  def priceNoFactory(bondid:String):Unit = {
    
    
	if (dbbonds.isEmpty) loadbonds
    val dbbond = dbbonds(bondid)
    
	val t1 = System.nanoTime
	val dates = DB.getParamSets(dbbond.issuedate, dbbond.maturity).toSet
    val bond:QLBond = JGBRConstructor(dbbond, dates.head._2).orNull
	val bondprices = dates.map {case (paramset, valuedate) => {
		setJGBRPricingEngine(bond, valuedate)
		val price = BondPrice(bond, valuedate, 1.0, paramset, null)
		println(bondid + " " + paramset + " " + price)
		price
	}}
    
	val t2 = System.nanoTime
    println("\n" + bondid + " - created " + bondprices.size + " prices - ")	
	println("%.3f sec".format(((t2 - t1)/1000000000.0)))
	storedprice ++= bondprices
//	push(bondprices)
//	addcount
  }
 
}

