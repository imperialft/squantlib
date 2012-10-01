package squantlib.pricingengines.bond

import squantlib.database.DB
import org.jquantlib.QL
import org.jquantlib.instruments.Bond
import org.jquantlib.time.{Date => qlDate}

class JGBRBondEngine(val valuationDate:qlDate) extends Bond.EngineImpl {
  
	var storedprice:Option[Double] = None
	
    override def calculate() /* @ReadOnly */ = {
	    var r = results_.asInstanceOf[Bond.ResultsImpl]
	  	r.value = storedprice.getOrElse {
	        val a = arguments_.asInstanceOf[Bond.ArgumentsImpl]
	        val vd = valuationDate.longDate
	        val (currentcpns, lastcpns) = DB.getCurrentAndPreviousCoupons(a.bondid, vd, 2)
	        val accrued = currentcpns.map(_.accruedCoupon(vd).getOrElse(Double.NaN)).sum
	        val previous = lastcpns.map(_.fixedamount.getOrElse(Double.NaN)).sum
	        storedprice = Some((1 + accrued - previous)*100)
	        storedprice.get
	    }
        r.settlementValue = r.value
    }

}
