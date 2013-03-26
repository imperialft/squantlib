package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.ScheduledPayoffs
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import squantlib.database.DB
import squantlib.model.rates.DiscountCurve

case class JGBRModel(bond:Bond, valueDate:qlDate) extends PricingModel {
  
	override def price:List[Double] = List.empty
	
	override val scheduledPayoffs:ScheduledPayoffs = ScheduledPayoffs.empty
	
	var storedPrice:Option[Double] = None
	
	override def discountedPrice(curve:DiscountCurve):Option[Double] = {
	  if (valueDate ge bond.maturity) None
	  else {
	    if (storedPrice.isEmpty) {
	      val accrued:Double = bond.accruedAmount.getOrElse(Double.NaN)
	      val previous:Double = bond.spotFixedAmountAll.filter{case (payday, _) => payday lt valueDate} match {
	      	case legs if legs.isEmpty => 0.0
	      	case legs if legs.size == 1 => legs.head._2
	      	case legs => (legs.sortBy{case (payday, _) => payday} takeRight 2).map(_._2).sum
	      	}
	      storedPrice = Some(1.0 + accrued - previous)
	    }
	    storedPrice
	  }
	}
}
	


object JGBRModel {
	
	def apply(market:Market, bond:Bond):Option[JGBRModel] = Some(JGBRModel(bond, market.valuedate))
	
}



