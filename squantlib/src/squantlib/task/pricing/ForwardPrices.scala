package squantlib.task.pricing

import squantlib.database.{DB, QLDB}
import squantlib.database.schemadefinitions.ForwardPrice
import squantlib.setting.initializer.{Calendars, RateConventions}
import squantlib.setting.PricingConvention
import squantlib.database.objectconstructor.{FixedRateBond, JGBRFixedBond, JGBRFloatBond}
import squantlib.instruments.bonds.{JGBFixedBond => sJGBFixedBond, JGBFloatBond => sJGBFloatBond}
import squantlib.model.fx.FX
import org.jquantlib.time.TimeSeries
import org.jquantlib.time.{ Date => qlDate }
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond}
import java.lang.{Double => JavaDouble}
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}

object ForwardPrices { 
  
  var storedprice = new HashSet[ForwardPrice] with SynchronizedSet[ForwardPrice]
  
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
  
  def defaultCurrencies:Set[String] = DB.getFXlist & RateConventions.keySet
  def defaultFXpairs:Set[(String, String)] = defaultCurrencies.map(fx => (fx, "JPY"))
  def defaultBonds:Set[String] = DB.getLatestBondPriceIDs
  
  def nextParamSet:String = DB.getLatestBondPriceParam match {
    case None => null
    case Some(p) => p._1
  }
  
  def currentParamSets:Set[String] = DB.getForwardPriceParams
  def updated:Boolean = currentParamSets contains nextParamSet 
  
  def update:Unit = {
    pricefx(nextParamSet, defaultFXpairs)
    pricebonds(nextParamSet, defaultBonds)
  }
  
  def pricebonds(paramset:String, bondid:Traversable[String] = null):Unit = {
	val factory = QLDB.getDiscountCurveFactory(paramset).orNull
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
		val fx = factory.getFX(bond.currency.code, "JPY").orNull
		val originalengine = bond.getPricingEngine
		val cdr = bond.calendar
		val simulstart:Long = factory.valuedate.serialNumber
		val simulend:Long = bond.maturityDate.serialNumber
		val alldates:Iterable[qlDate] = (for(d <- simulstart to simulend) yield (new qlDate(d)))
		val simuldates = alldates.filter(cdr.isBusinessDay).toSet
		val initialfx = bond.initialFX
		
		val pricelist:Set[ForwardPrice] = simuldates.map(d => {
			PricingConvention.setAdjustedPricingEngine(bond, factory, d)
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
		)
		
		bond.setPricingEngine(originalengine, factory.valuedate)
		
		println("Bond: " + bond.bondid + " "  + bond.currency + " " + bond.maturityDate.shortDate + 
		    " results:" + (if (pricelist != null) pricelist.size else "N/A") + " lastprice:" + (
		    if (pricelist == null || pricelist.isEmpty) "N/A" else (pricelist.maxBy(_.valuedate).value + "-" + pricelist.maxBy(_.valuedate).valuedate)) + 
		    " vs redem:" + bond.redemption.amount)
		
		pricelist
	}}.seq.flatten;
	
	storedprice ++= forwardprices
  }

  def pricefx(paramset:String, fxpairs:Set[(String, String)] = null):Unit = {
	val factory = QLDB.getDiscountCurveFactory(paramset).orNull
	if (factory == null || factory.curves.size == 0) {
	  println("Curve not found")
	  return
	}
	  
	val fxs:Set[FX] = fxpairs.map(f => factory.getFX(f._1, f._2)).flatMap(s => s)
	
	println("valuedate :\t" + factory.valuedate.shortDate + " paramset :\t" + paramset)
	println("\n* Market *")
	println(factory.describe)
	
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val forwardprices:Set[ForwardPrice] = fxs.par.map { fx => {
		val cdr = Calendars(fx.currency2.code).get
		val simulstart = factory.valuedate.serialNumber
		val simulend = factory.valuedate.serialNumber + fx.maxdays.toInt
		val alldates:Iterable[qlDate] = for (d <- simulstart to simulend) yield (new qlDate(d))
		val simuldates = alldates.filter(cdr.isBusinessDay(_)).toSet
		
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
		}
		
		println("FX: " + fx.name +  " spot:" + fx.spot +
		    " results:" + (if (pricelist != null) pricelist.size else "N/A") + " lastprice:" + (
		    if (pricelist == null || pricelist.isEmpty) "N/A" else (pricelist.maxBy(_.valuedate).value + " - " + pricelist.maxBy(_.valuedate).valuedate)))
		
		pricelist
	}}.seq.flatten;
	
	storedprice ++= forwardprices
  }
  
  
    
}

