package squantlib.setting

import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.database.objectconstructor._
import squantlib.instruments.bonds._
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond}
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import java.util.{GregorianCalendar, Calendar}


object PricingConvention {
	 
	def bondConstructor(dbbond:dbBond, factory:DiscountCurveFactory):Option[qlBond] = 
	  dbbond match {
	    case p if JGBRFixedBond.isCompatible(p) => JGBRFixedBond(p, factory.valuedate)
		case p if JGBRFloatBond.isCompatible(p) => JGBRFloatBond(p, factory.valuedate)
		case p if FixedRateBond.isCompatible(p) => FixedRateBond(p, factory)
		case _ => None
		}
	
	def priceFrom(dbbond:dbBond):Option[JavaDate] = 
	  dbbond match {
	    case p if JGBRFixedBond.isCompatible(p) => Some(p.issuedate)
		case p if JGBRFloatBond.isCompatible(p) => Some(p.issuedate)
		case p if FixedRateBond.isCompatible(p) => Some(addDays(p.issuedate, -400))
		case _ => None
	  }
	
	def bondPriceFrom(bond:qlBond):Option[qlDate] = 
	  bond match {
	    case p:JGBFixedBond => Some(p.issueDate)
		case p:JGBFloatBond => Some(p.issueDate)
		case p:qlFixedRateBond => Some(p.issueDate.sub(265))
		case _ => None
	  }
	
	private def addDays(d:JavaDate, days:Int):JavaDate = {
	  val gc:GregorianCalendar = new GregorianCalendar
	  gc.setTime(d)
	  gc.add(Calendar.DAY_OF_YEAR, days)
	  gc.getTime
	}
	
	def setAdjustedPricingEngine(bond:qlBond, factory:DiscountCurveFactory, valuedate:qlDate):Unit = 
		bond match {
		  case b:JGBFixedBond => { // Use spot price for now
								    //JGBRFixedBond.setAdjustedPricingEngine(b, d)
								  }
		  case b:JGBFloatBond => {// Use spot price for now
									//JGBRFloatBond.setAdjustedPricingEngine(b, d)
								  }
		  case b:qlFixedRateBond => FixedRateBond.setAdjustedPricingEngine(b, factory, valuedate)
		  case _ => {}
		}
	

}