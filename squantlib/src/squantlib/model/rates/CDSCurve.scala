package squantlib.model.rates

import scala.collection.immutable.{TreeMap, SortedMap}
import squantlib.database.schemadefinitions.CDSParameter
import squantlib.model.yieldparameter.YieldParameter
import squantlib.setting.initializer.Currencies
import org.jquantlib.time.{Period => qlPeriod, Date => qlDate}
import org.jquantlib.currencies.Currency
import squantlib.model.yieldparameter.{FlatVector, LinearNoExtrapolation}


/**
 * @constructor stores each information 
 * @param floatindex can take any maturity.
 */
case class CDSCurve(val rate:YieldParameter, val currency:Currency, val issuerid:String) extends AbstractCurve{
  def this(r:YieldParameter, c:String, id:String) = this(r, Currencies.getOrElse(c, null), id)
  
  override def shifted(v:(Double, Double) => Double):CDSCurve = new CDSCurve(rate.shifted(v), currency, issuerid)
}

object CDSCurve{
  
	/**
	 * Constructs CDScurve from CDSParameter per each combination of issuerid, currency, paramset.
	 * @param set of CDSParameter
	 * @returns map from (issuerid, Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getallcurves(params:Set[CDSParameter], valuedate:qlDate):Map[(String, String), CDSCurve] = {
  	  val cdsgroups = params.groupBy(p => (p.issuerid, p.currencyid))
  	   
  	  cdsgroups.withFilter{case ((_, ccy), _) => Currencies.contains(ccy) }
  	  	.map{ case ((issuer, ccy), v) => 
  		  val cdscurve = curveconstructor(valuedate, TreeMap(v.toSeq.map(p => (new qlPeriod(p.maturity), p.spread / 10000.0)) :_*))
  		  ((issuer, ccy), new CDSCurve(cdscurve, Currencies(ccy).get, issuer))
  	  	}
  	}
  
	/**
	 * Constructs CDScurve from one CDSParameter as flat spread.
	 */
  	def getcurve(p:CDSParameter):CDSCurve = 
  	  new CDSCurve(new FlatVector(new qlDate(p.paramdate), p.spread), Currencies(p.currencyid).get, p.issuerid)

	/**
	 * Constructs CDScurve from CDSParameter per each combination of issuerid, currency, paramset.
	 * @param set of CDSParameter
	 * @returns map from issuerid to LiborDiscountCurve
	 */
  	def getcurves(params:Set[CDSParameter], valuedate:qlDate):Iterable[CDSCurve] = {
  	  val curves = getallcurves(params, valuedate).groupBy(c => c._1._1)
  	  curves.map(c => c._2.getOrElse((c._1, defaultccy), c._2.head._2))
  	}
  	
  	private def defaultccy = "USD"
  	  
	def curveconstructor(valuedate:qlDate, values:SortedMap[qlPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case _ => new LinearNoExtrapolation(valuedate, values)
			}
  	 
  	def apply(params:Set[CDSParameter], valuedate:qlDate):Iterable[CDSCurve] = getcurves(params, valuedate)
  	
  	def apply(params:Set[CDSParameter]):Iterable[CDSCurve] = if (params.isEmpty) Set.empty else apply(params, new qlDate(params.head.paramdate))

}