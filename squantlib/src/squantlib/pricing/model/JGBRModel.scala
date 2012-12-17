package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.model.Bond
import org.jquantlib.time.{Date => qlDate}
import java.util.{Date => JavaDate}
import squantlib.database.DB
import squantlib.model.rates.DiscountCurve

case class JGBRModel(bond:Bond, valueDate:qlDate) extends PricingModel {
  
	override val isPricedByLegs = false
	
	override def price:List[Double] = List.empty
	
	override val periods:List[CalcPeriod] = List.empty
	
	override val payoff:List[Payoff] = List.empty
	
	var storedPrice:Option[Double] = None
	
	override def discountedPrice(curve:Option[DiscountCurve]):Option[Double] = {
	  if (valueDate ge bond.maturity) None
	  else {
	    if (storedPrice.isEmpty) {
	      val accrued:Double = bond.accruedAmount.getOrElse(Double.NaN)
	      val previous:Double = bond.spotFixedAmountAll.filter(_._1.paymentDate lt valueDate) match {
	      	case legs if legs.isEmpty => 0.0
	      	case legs if legs.size == 1 => legs.head._2
	      	case legs => (legs.sortBy(_._1.paymentDate) takeRight 2).map(_._2).sum
	      	}
	      storedPrice = Some(1.0 + accrued - previous)
	    }
	    storedPrice
	  }
	}
}
	


object JGBRModel {
	
	def apply(market:Market, bond:Bond):Option[JGBRModel] = {
	  Some(JGBRModel(bond, market.valuedate))
	}
}



