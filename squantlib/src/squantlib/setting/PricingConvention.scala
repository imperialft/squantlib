package squantlib.setting

import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.model.{Market, Bond}
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond}
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import java.util.{GregorianCalendar, Calendar}


object PricingConvention {
	 
	def priceFrom(bond:Bond):qlDate = bond.issueDate.sub(400)

	def priceFromQL(bond:dbBond):Option[JavaDate] = Some(addDays(bond.issuedate, -400))
	
	def bondPriceFrom(bond:qlBond):Option[qlDate] = Some(bond.issueDate.sub(265))
	
	private def addDays(d:JavaDate, days:Int):JavaDate = {
	  val gc:GregorianCalendar = new GregorianCalendar
	  gc.setTime(d)
	  gc.add(Calendar.DAY_OF_YEAR, days)
	  gc.getTime
	}
	

}