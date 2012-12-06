package squantlib.pricing.model

import squantlib.model.Market
import squantlib.payoff.{Payoff, Payoffs, Schedule, CalcPeriod}
import squantlib.pricing.mcengine._
import squantlib.model.fx.FX
import squantlib.model.Bond
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import squantlib.model.rates.DiscountCurve
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import org.jquantlib.daycounters.Actual365Fixed


case class FXMontecarlo1f(val valuedate:qlDate, 
					  val mcengine:Montecarlo1f, 
					  val ipayoffs:Payoffs, 
					  val ischedule:Schedule,
					  val fx:FX,
					  var mcPaths:Int) extends PricingModel {
  
	assert (ipayoffs.size == ischedule.size, "assertion failed : payoffsize=" + ipayoffs.size + " vs schedule.size=" + ischedule.size)
	assert (ipayoffs.variables.size == 1, "assertion failed : variables=" + ipayoffs.variables.size)
	
	val (fixlist, mclist) = (ischedule, ipayoffs).zipped.toList.partition{case (e, p) => p.variables.size == 0}
	
	val (fixPeriods, fixPayoff) = fixlist.unzip
	val fixPayoffs = Payoffs(fixPayoff)
	
	def fixPrice:List[Double] = fixPayoffs.map(_.price).toList
	
	val (mcPeriods, mcPayoff) = mclist.unzip
	val mcPayoffs = Payoffs(mcPayoff)
	val mcDates = mcPeriods.map(_.eventDate)
	val mcYears = Schedule(mcPeriods).eventYears(valuedate)

	def mcPaths(paths:Int):List[List[Double]] = {
	  val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths)
	  if (mcdates.sameElements(mcYears)) mcpaths
	  else { println("invalid mc dates"); List.empty}
	}
	
	def mcPrice(paths:Int):List[Double] = 
	  mcPaths(paths).map(mcPayoffs.price).transpose.map(_.sum / paths.toDouble)
	
	def modelForward(paths:Int):List[Double] = 
	  mcPaths(paths).transpose.map(_.sum).map(_ / paths)
	
	def fxForward:List[(qlDate, Double)] = mcDates zip mcDates.map(fx.forward)
	
	def vol:List[(qlDate, Double)] = mcDates zip mcDates.map(fx.volatility)
	 
	def checkForward(paths:Int):Unit = 
	  printf("date, MC, forward\n" + (mcDates zip modelForward(paths)).map{case (d, fwd) => { 
	        val fxfwd = fx.forward(d); (d, fwd, fxfwd, fwd - fxfwd)}}.mkString("\n"))
	
	def price:List[Double] = price(mcPaths)
	
	def price(paths:Int):List[Double] = fixPrice ++ mcPrice(paths)
	
	val periods:List[CalcPeriod] = fixPeriods ++ mcPeriods
	
	val payoff:List[Payoff] = fixPayoff ++ mcPayoffs
	
}


object FXMontecarlo1f {
	
	var defaultPaths = 100000
	
	def apply(market:Market, bond:Bond):Option[FXMontecarlo1f] = apply(market, bond, defaultPaths)
  
	def apply(market:Market, bond:Bond, paths:Int):Option[FXMontecarlo1f] = {
			
	  val valuedate = market.valuedate
	  
	  val (schedule, payoffs) = bond.livePayoffs(valuedate)
	  if (payoffs.variables.size != 1) { println(bond.id + " : payoff not compatible with FX1d model"); return None}
	  
	  val variable = payoffs.variables.head
	  val fx = market.getFX(variable).orNull
	  if (fx == null) {println(bond.id + " : invalid fx underlying - " + variable + " in market " + market.paramset); return None}
	  if (fx.currencyDom != bond.currency) {println(bond.id + " : quanto model not supported - " + variable); return None}
	  
	  val enginename = try { bond.settings.get.parseJsonString("mcengine") } catch {case _ => null}
	  val mcmodel:Montecarlo1f = (enginename match {
	    case "FXzeroVol" => FXzeroVol1f(fx)
	    case "FXBlackScholes1f" => FXBlackScholes1f(fx)
	    case null => FXBlackScholes1f(fx) // default
	    case _ => None
	  }).orNull
	  if (mcmodel == null) {println(bond.id + " : model name not found or model calibration error"); return None}
	  
	  Some(new FXMontecarlo1f(valuedate, mcmodel, payoffs, schedule, fx, paths))
	}
}

