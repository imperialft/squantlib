package squantlib.database.objectconstructor

import scala.collection.immutable.{TreeMap, SortedMap}
import squantlib.database.schemadefinitions.CDSParameter
import squantlib.model.discountcurve.{CDSCurve, LiborDiscountCurve}
import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.model.currencies.CurrencyConversion
import org.jquantlib.time.{Period => JPeriod, Date => JDate}
import org.jquantlib.currencies.Currency
import squantlib.parameter.yieldparameter.{FlatVector, LinearNoExtrapolation}


object CDSCurveConstructor {

	def curveconstructor(valuedate:JDate, values:SortedMap[JPeriod, Double]):YieldParameter
		= (values.keySet.size) match {
			case 1 => new FlatVector(valuedate, values)
			case _ => new LinearNoExtrapolation(valuedate, values)
			}
  
	/**
	 * Constructs CDScurve from CDSParameter per each combination of issuerid, currency, paramset.
	 * @param set of CDSParameter
	 * @returns map from (issuerid, Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getcurves(params:Set[CDSParameter]):Map[(String, String, String), CDSCurve] = {
  	  val cdsgroups = params.groupBy(p => (p.issuerid, p.currencyid, p.paramset))
  	  
  	  cdsgroups.map{ case ((issuer, ccy, pset), v) => {
  		  val valuedate = new JDate(v.head.paramdate)
  		  val cdscurve = curveconstructor(valuedate, TreeMap(v.toSeq.map(p => (new JPeriod(p.maturity), p.spread / 10000.0)) :_*))
  		  ((issuer, ccy, pset), new CDSCurve(cdscurve, CurrencyConversion.getcurrency(ccy)))
  	  	}}
  	}
  	
	/**
	 * Constructs CDScurve from one CDSParameter as flat spread.
	 */
  	def getcurve(p:CDSParameter):CDSCurve = 
  	  new CDSCurve(new FlatVector(new JDate(p.paramdate), p.spread), CurrencyConversion.getcurrency(p.currencyid))

	/**
	 * Constructs CDScurve from CDSParameter from specific paramset.
	 * In case of more than one currency for one issuer, USD is chosen if exists, else first on the list.
	 * @param set of CDSParameter
	 * @returns map from (issuerid, Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getcurves(params:Set[CDSParameter], paramset:String):Map[String, CDSCurve] = {
  	  val curves = getcurves(params.filter(p => p.paramset == paramset)).groupBy(c => c._1._1)
  	  curves.map(c => (c._1, c._2.getOrElse((c._1, defaultccy, paramset), c._2.head._2)))
  	}
  	
  	private def defaultccy = "USD"
} 

