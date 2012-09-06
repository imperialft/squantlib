package squantlib.task.pricing

import squantlib.database.{DB, QLDB}
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.ForwardPrice
import org.jquantlib.time.{ Date => qlDate }
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import org.jquantlib.instruments.{Bond => qlBond}
import squantlib.initializer.{Calendars, RateConvention}

object ForwardPrices { 
  
  var storedprice = new HashSet[ForwardPrice] with SynchronizedSet[ForwardPrice]
  private var storedts = new HashMap[String, TimeSeries[JavaDouble]] with SynchronizedMap[String, TimeSeries[JavaDouble]]
  
  def push:Unit =  {
    if (storedprice.size != 0) {
    	printf("Extracting valid price ..")
		storedprice.retain(!_.value.isNaN)
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(storedprice, false)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		storedprice.clear
		}
	}
  
  def clear:Unit = storedprice.clear
  
  def defaultCurrencies:Set[String] = DB.getFXlist & RateConvention.allConventions.map(_._1).toSet
  def defaultFXpairs:Set[(String, String)] = defaultCurrencies.map(fx => (fx, "JPY"))
  def defaultBonds:Set[String] = DB.latestPrices.map(_.bondid)
  
  def nextParamSet:String = DB.latestPriceParam._1
  def currentParamSets:Set[String] = DB.getForwardPriceParams
  def updated:Boolean = currentParamSets contains nextParamSet 
  
  def update:Unit = {
    pricefx(nextParamSet, defaultFXpairs)
    pricebonds(nextParamSet, defaultBonds)
  }
  
  def pricebonds(paramset:String, bondid:Traversable[String] = null):Unit = {
	val factory = QLDB.getDiscountCurveFactory(paramset)
	if (factory == null || factory.curves.size == 0) {
	  println("Curve not found")
	  return
	}
	  
	val bonds:Set[qlBond] = if (bondid == null) QLDB.getBonds(factory) else QLDB.getBonds(bondid, factory)
	
	val isvalidbond = bonds map {b => 
		(b.bondid, try { b.cleanPrice; true } catch { case e => false })
	} toMap;
	
	println("valuedate :\t" + factory.valuedate.shortDate + " paramset :\t" + paramset)
	println("\n* Market *")
	println(factory.describe)
	
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val forwardprices:Set[ForwardPrice] = bonds.filter(b => isvalidbond(b.bondid)).par.map { bond => {
		val fx = factory.getFX(bond.currency.code, "JPY")
		val originalengine = bond.getPricingEngine
		val cdr = bond.calendar
		val simulstart = factory.valuedate.serialNumber
		val simulend = bond.maturityDate.serialNumber
		val simuldates = (simulstart to simulend) map (new qlDate(_)) filter(cdr.isBusinessDay(_))
		val initialfx = bond.initialFX
		
		val pricelist = simuldates map { d => {
			val newengine = factory.getcustomdiscountbondengine(bond, cdr, d)
			bond.setPricingEngine(newengine, d)
			val cprice = try bond.cleanPrice catch { case e => Double.NaN }
			val fxvalue = try fx.forwardfx(d) catch { case e => Double.NaN }
			new ForwardPrice (
			    id = "BOND:" + bond.bondid + ":" + paramset + ":" + ("%tY%<tm%<td" format d.longDate),
			    paramset = paramset,
				paramdate = factory.valuedate.longDate,
				valuedate = d.longDate,
				underlying = "BOND:" + bond.bondid,
				value = cprice,
				valuejpy = if (initialfx == 0 || cprice.isNaN || fxvalue.isNaN) None 
							else Some(cprice * fxvalue / initialfx),
				created = Some(currenttime)
			    )
			}
		} toSet;
		
		bond.setPricingEngine(originalengine, factory.valuedate)
		
		println("Bond: " + bond.bondid + " "  + bond.currency + " " + bond.maturityDate.shortDate + 
		    " results:" + (if (pricelist != null) pricelist.size else "N/A") + " lastprice:" + (
		    if (pricelist == null || pricelist.isEmpty) "N/A" else (pricelist.maxBy(_.valuedate).value + "-" + pricelist.maxBy(_.valuedate).valuedate)) + 
		    " vs redem:" + bond.redemption.amount)
		
		pricelist
	}}.seq.flatten;
	
	storedprice ++= forwardprices
  }

  def pricefx(paramset:String, fxpairs:Traversable[(String, String)] = null):Unit = {
	val factory = QLDB.getDiscountCurveFactory(paramset)
	if (factory == null || factory.curves.size == 0) {
	  println("Curve not found")
	  return
	}
	  
	val fxs = fxpairs.map(f => factory.getFX(f._1, f._2)).toSet.filter(_ != null)
	
	println("valuedate :\t" + factory.valuedate.shortDate + " paramset :\t" + paramset)
	println("\n* Market *")
	println(factory.describe)
	
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val forwardprices:Set[ForwardPrice] = fxs.par.map { fx => {
		val cdr = Calendars(fx.currency2)
		val simulstart = factory.valuedate.serialNumber
		val simulend = factory.valuedate.serialNumber + fx.maxdays.toInt
		val simuldates = (simulstart to simulend) map (new qlDate(_)) filter(cdr.isBusinessDay(_))
		
		val pricelist = simuldates map { d => {
			val fxvalue = try fx.forwardfx(d) catch { case e => Double.NaN }
			new ForwardPrice (
			    id = "FX:" + fx.name + ":" + paramset + ":" + ("%tY%<tm%<td" format d.longDate),
			    paramset = paramset,
				paramdate = factory.valuedate.longDate,
				valuedate = d.longDate,
				underlying = "FX:" + fx.name,
				value = fxvalue,
				valuejpy = None,
				created = Some(currenttime)
			    )
			}
		} toSet;
		
		println("FX: " + fx.name +  " spot:" + fx.spot +
		    " results:" + (if (pricelist != null) pricelist.size else "N/A") + " lastprice:" + (
		    if (pricelist == null || pricelist.isEmpty) "N/A" else (pricelist.maxBy(_.valuedate).value + " - " + pricelist.maxBy(_.valuedate).valuedate)))
		
		pricelist
	}}.seq.flatten;
	
	storedprice ++= forwardprices
  }
  
  
    
}

