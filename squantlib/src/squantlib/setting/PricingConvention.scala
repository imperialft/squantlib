package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.model.discountcurve.DiscountCurveFactory
import org.jquantlib.instruments.{Bond => qlBond}
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
	
	def addDays(d:JavaDate, days:Int):JavaDate = {
	  val gc:GregorianCalendar = new GregorianCalendar
	  gc.setTime(d)
	  gc.add(Calendar.DAY_OF_YEAR, days)
	  gc.getTime
	}
	

}