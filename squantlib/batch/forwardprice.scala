
/**
 * Construct forward price of bonds and FX
 */

//val paramset = "20120508-000"
//val factory = QLDB.getDiscountCurveFactory(paramset)
//val bond = QLDB.getBonds(List("KEXIM-00022"), factory).head
//val originalengine = bond.getPricingEngine
//val cdr = bond.calendar
//val simulstart = factory.valuedate.serialNumber
//val simulend = bond.maturityDate.serialNumber
//val simuldates = (simulstart to simulend) map (new Date(_)) filter(cdr.isBusinessDay(_))
//
//val pricelist = simuldates map { d => {
//	val newengine = factory.getcustomdiscountbondengine(bond, cdr, d)
//	bond.setPricingEngine(newengine, d)
//	(d, bond.cleanPrice)
//}}
//
//bond.setPricingEngine(originalengine, factory.valuedate)
//


import squantlib.task.pricing.ForwardPrices

val paramset = DB.latestParamSet

val currencies = DB.getFXlist & squantlib.initializer.RateConvention.getConvention.map(_._1).toSet
val fxpairs = currencies.map(fx => (fx, "JPY"))
ForwardPrices.pricefx(paramset, fxpairs)
ForwardPrices.push

val bondids = QLDB.getBonds.map(_.bondid) 
ForwardPrices.pricebonds(paramset, bondids)
ForwardPrices.push

