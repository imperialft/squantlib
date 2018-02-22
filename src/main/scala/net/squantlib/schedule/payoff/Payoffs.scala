package net.squantlib.schedule.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import scala.collection.JavaConverters._
import scala.Predef._
import net.squantlib.schedule.FixingLegs
import scala.Predef.{DummyImplicit => DI}
import scala.language.postfixOps
import net.squantlib.schedule.call._

case class Payoffs(payoffs:List[Payoff]) extends LinearSeq[Payoff] with FixingLegs[Payoff] {
  
	val underlyings:Set[String] = {

	  @tailrec def variablesRec(paylist:List[Payoff], acc:Set[String]):Set[String] = {
			if (paylist.isEmpty) acc
			else variablesRec(paylist.tail, paylist.head.variables ++ acc)
	  }

	  variablesRec(payoffs, Set.empty)
	}
	
	val factors:Int = underlyings.size
	
	val isPriceable:Boolean = payoffs.forall(_.isPriceable)
	
	override val fixinglegs = payoffs

	def isPaymentFixed:Boolean = payoffs.forall(_.isPaymentFixed)
	
//	abstract class FixingInterpreter[T, U] {
//	  def price(fixing:T, payoff:Payoff, pastPayments:List[Double]):Double
//
//	  def triggered(fixing:U, trigger:Option[U], triggerUp:Boolean):Boolean = trigger.collect{case t => triggered(fixing, t, triggerUp)}.getOrElse(false)
//
//    def triggered(fixing:U, trigger:U, triggerUp:Boolean):Boolean
//
//    def triggeredUnderlyings(fixing:U, trigger:U, triggerUp:Boolean):Set[String]
//
//	  def terminationAmount(fixing:T, trigNominal:Double, forwardStrikes:Option[U]):Double
//
//	  def assignFixings(fixing:T, payoff:Payoff, pastPayments:List[Double]):Unit
//
//		def assignSettlementFixings(fixing:T, payoff:Payoff):Unit
//	}
//
//  implicit object ListMapList extends FixingInterpreter[List[Map[String, Double]], Map[String, Double]] {
//    def price(fixing:List[Map[String, Double]], p:Payoff, pastPayments:List[Double]) = if (fixing.isEmpty) p.price else p.price(fixing, pastPayments)
//
//    def triggeredUnderlyings(fixing:Map[String, Double], trigger:Map[String, Double], triggerUp:Boolean) = {
//      trigger.filter{case (ul, v) =>
//        if (triggerUp) fixing.get(ul).collect{case fv => fv >= v}.getOrElse(false)
//        else fixing.get(ul).collect{case fv => fv <= v}.getOrElse(false)
//      }.keySet
//    }
//
//    def triggered(fixing:Map[String, Double], trigger:Map[String, Double], triggerUp:Boolean) =
//      !trigger.isEmpty && triggeredUnderlyings(fixing, trigger, triggerUp).size == trigger.size
//
//    //    def triggered(fixing:List[Map[String, Double]], trigger:Option[Map[String, Double]], triggerUp:Boolean) = trigger match {
////      case None => false
////      case Some(t) if t.isEmpty => false
////      case Some(t) if fixing.lastOption.collect{case f => f.isEmpty}.getOrElse(true) => false
////      case Some(t) => t.forall{case (v, d) =>
////        if(triggerUp) fixing.last.get(v).collect{case vv => vv >= d}.getOrElse(false)
////        else fixing.last.get(v).collect{case vv => vv <= d}.getOrElse(false)
////      }
////    }
//
//    def terminationAmount(fixing:List[Map[String, Double]], trigNominal:Double, forwardStrikes:Option[Map[String, Double]]) = forwardStrikes match {
//      case Some(k) => trigNominal * k.map{case (k, v) => fixing.last(k) / v}.min
//      case _ => trigNominal
//    }
//
//    def assignFixings(fixing:List[Map[String, Double]], payoff:Payoff, pastPayments:List[Double]) = {
//      if (payoff.physical && fixing.size > 1) {
//        payoff.assignFixings(fixing(fixing.length - 2))
//      } else {
//        payoff.assignFixings(fixing.last)
//      }
//    }
//
//    def assignSettlementFixings(fixing:List[Map[String, Double]], payoff:Payoff) = payoff.assignSettlementFixings(fixing.last)
//  }

  /*
   * Returns price array, when there's no variable.
   */
  def price:List[Double] = payoffs.map(_.price)

  /*
 * Returns price array, to be used when there's more than one fixing dates per payoff
 * @param fixings market parameters as Map(variable name -> value) in order of event dates, in order of payoff.
 */

  def price(
    fixings:List[List[Map[String, Double]]],
    calls: List[Callability],
    dayCounts:List[Double],
    accruedPayment:Option[Double]
  )(implicit d1:DI, d2:DI, d3:DI):List[Double] = {
    assert(fixings.size == payoffs.size && fixings.size == calls.size, s"Number of fixings(${fixings.size}), calls(${calls.size}) and payoffs(${payoffs.size}) must match - fixings:${fixings} calls:${calls}")

    val trigList:List[Option[Map[String, Double]]] = calls.map(c => c.optionalTriggers)
    val trigUp:List[Boolean] = calls.map(_.triggerUp)
    val trigAmt:List[Double] = calls.map(_.bonusAmount)
    val forwardStrikes: List[Option[Map[String, Double]]] = calls.map(_.optionalForwardStrikes)
    val targets:List[Option[Double]] = calls.map(_.targetRedemption)
    val removeSatisfiedTriggers: List[Boolean] = calls.map(_.removeSatisfiedTriggers)

    priceTrig(payoffs, fixings, List.empty, trigList, trigUp, trigAmt, forwardStrikes, targets, removeSatisfiedTriggers, dayCounts, accruedPayment.getOrElse(0.0), false)
  }

  /*
 * Returns price array, to be used when there's more than one fixing dates per payoff and with trigger.
 * @param fixings market parameters as Map(variable name -> value) in order of payoff.
 */
  def price(
    fixings:List[List[Map[String, Double]]],
    trigger:List[Option[Map[String, Double]]],
    trigUp: List[Boolean],
    trigAmount:List[Double],
    forwardStrikes: List[Option[Map[String, Double]]],
    targets:List[Option[Double]],
    removeSatisfiedTriggers:List[Boolean],
    dayCounts:List[Double],
    accruedPayment:Option[Double]
  )(implicit d1:DI, d2:DI, d3:DI):List[Double] = {
    assert(fixings.size == payoffs.size && fixings.size == trigger.size, s"Number of fixings(${fixings.size}), trigger(${trigger.size}) and payoffs(${payoffs.size}) must match - fixings:${fixings} triggers:${trigger}")
    priceTrig(payoffs, fixings, List.empty, trigger, trigUp, trigAmount, forwardStrikes, targets, removeSatisfiedTriggers, dayCounts, accruedPayment.getOrElse(0.0), false)
  }

  @tailrec
  private def priceTrig(
    paylist:List[Payoff],
    fixlist:List[List[Map[String, Double]]],
    acc:List[Double],
    triglist:List[Option[Map[String, Double]]],
    trigUp:List[Boolean],
    trigamt:List[Double],
    forwardStrikes: List[Option[Map[String, Double]]],
    targets:List[Option[Double]],
    removeSatisfiedTriggers:List[Boolean],
    dayCounts:List[Double],
    accruedPayment:Double,
    triggered:Boolean
  ):List[Double] = (paylist, fixlist, triglist, trigUp, trigamt, forwardStrikes, targets, removeSatisfiedTriggers, dayCounts) match {

    case (Nil, _, _, _, _, _, _, _, _) => acc.reverse

    case _ if triggered => acc.reverse ++ List.fill(paylist.tail.size)(0.0)

    case (ph::pt, fh::ft, tlh::tlt, tuh::tut, tah::tat, fsh::fst, tgth::tgtt, memh::memt, dh::dt) =>
      val eventDateFixing = ph.getEventDateFixing(fh).getOrElse(Map.empty[String, Double])
      val couponRate = ph.price(fh, acc)
      if (tlh.collect{case trig => ph.isTriggered(eventDateFixing, trig, tuh)}.getOrElse(false)) { //((couponRate + trigamt.head) :: acc).reverse ++ List.fill(paylist.tail.size)(0.0)
        val trigAmount = (tah + 1.0) / dh
        ((couponRate + ph.terminationAmount(eventDateFixing, trigAmount, fsh)) :: acc).reverse ++ List.fill(pt.size)(0.0)
      }
      else {
        val paidAmount = accruedPayment + couponRate * dh
        tgth match {
          case Some(tgt) if paidAmount >= tgt => //((couponRate + trigamt.head)::acc).reverse ++ List.fill(paylist.tail.size)(0.0)
            val trigAmount = (tah + 1.0) / dh
            ((couponRate + ph.terminationAmount(eventDateFixing, trigAmount, fsh))::acc).reverse ++ List.fill(pt.size)(0.0)
          case _ =>
            val remainTrigger = if (memh) {
              tlh.collect { case trig =>
                val triggeredUls = ph.triggeredUnderlyings(eventDateFixing, trig, tuh)
                if (triggeredUls.isEmpty) tlt
                else tlt.map(tt => tt.collect { case fut => fut.filter{ case (kk, vv) => !triggeredUls.contains(kk)}})
              }.getOrElse(tlt)
            } else tlt

            priceTrig(pt, ft, couponRate :: acc, remainTrigger, tut, tat, fst, tgtt, memt, dt, paidAmount, false)
        }
      }
    case _ => acc.reverse ++ List.fill(paylist.tail.size)(Double.NaN)
  }

	def ++(another:Payoffs) = new Payoffs(payoffs ++ another.payoffs)
	
	def :+(payoff:Payoff) = new Payoffs(payoffs :+ payoff)
	
	override def toString = payoffs.map(_.toString).mkString("\n")
	
	def apply(i:Int):Payoff = payoffs(i)
    
	override def isEmpty:Boolean = payoffs.isEmpty
	
	override def head:Payoff = payoffs.head
	
	override def tail = payoffs.tail
	
	override def length = payoffs.length
	
	override def iterator:Iterator[Payoff] = payoffs.iterator
	
	override def toList:List[Payoff] = payoffs
	
	override def size:Int = payoffs.size
	
	def reorder(order:List[Int]) = new Payoffs((0 to payoffs.size-1).toList.map(i => payoffs(order(i))))
	
	val jsonString:String = payoffs.map(_.jsonString).mkString(";")
}


object Payoffs {
	
	def empty:Payoffs = new Payoffs(List.empty)
	
	def apply(payoffs:LinearSeq[Payoff]) = new Payoffs(payoffs.toList)
	
	def apply(formula:String, legs:Int = 1)(implicit fixingInfo:FixingInformation):Option[Payoffs] =	{
	  if (legs == 0) Some(Payoffs(List.empty))
	  else if (formula == null || formula.trim.isEmpty) {
	    def getNullPayoff = new NullPayoff()
	    Some(Payoffs(List.fill(legs)(getNullPayoff)))
	  }
	  else {
	    val payofflist:List[Payoff] = formula.jsonNode match {
	      case Some(n) if n.isArray && n.size > 0 => n.getElements.asScala.toList.map(f => getPayoff(toJsonString(f)))
	      case _ => formula.split(";").toList.map(getPayoff)
	    }
	    
	    def getFirstElement:Payoff = formula.jsonNode match {
	      case Some(n) if n.isArray && n.size > 0 => getPayoff(toJsonString(n.getElements.asScala.toList.head))
	      case _ => getPayoff(formula.split(";").head)
	    }
	  
	  	val fullpayoff = if (payofflist.size < legs) List.fill(legs - payofflist.size)(getFirstElement) ++ payofflist else payofflist
	  	Some(Payoffs(fullpayoff))
	}}
	
	def toJsonString(n:JsonNode):String = (new ObjectMapper).writeValueAsString(n)
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type").orNull
	  }
	
	def getPayoff(f:String)(implicit fixingInfo:FixingInformation):Payoff = Payoff(f).getOrElse(GeneralPayoff(f))
	  
}
