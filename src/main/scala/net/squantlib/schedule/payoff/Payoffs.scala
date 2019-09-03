package net.squantlib.schedule.payoff

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper

import scala.collection.LinearSeq
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.{FixingInformation, JsonUtils, UnderlyingFixing}
import net.squantlib.util.JsonUtils._

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

	val paymentFXUnderlyings:Set[String] = {
		payoffs.filter(p => p.currencyId != p.paymentCurrencyId).map(p => p.currencyId + p.paymentCurrencyId).toSet
	}
	
	val factors:Int = underlyings.size
	
	val isPriceable:Boolean = payoffs.forall(_.isPriceable)
	
	override val fixinglegs = payoffs

	def isPaymentFixed:Boolean = payoffs.forall(_.isPaymentFixed)

  /*
   * Returns price array, when there's no variable.
   */
  def price:List[Double] = payoffs.map(_.price)

  /*
 * Returns price array, to be used when there's more than one fixing dates per payoff
 * @param fixings market parameters as Map(variable name -> value) in order of event dates, in order of payoff.
 */

  def price(
    fixingList:List[List[UnderlyingFixing]],
		callFixingList:List[Option[UnderlyingFixing]],
    calls: List[Callability],
    dayCounts:List[Double],
    accruedPayment:Option[Double]
  )(implicit d1:DI, d2:DI, d3:DI):List[Double] = {
    assert(fixingList.size == payoffs.size && fixingList.size == calls.size, s"Number of fixingList(${fixingList.size}), calls(${calls.size}) and payoffs(${payoffs.size}) must match - fixingList:${fixingList} calls:${calls}")

    val triggerStrikes:List[Option[UnderlyingFixing]] = calls.map(c => c.optionalTriggers.collect{case vs => vs})
    val trigUp:List[Boolean] = calls.map(_.triggerCondition.triggerUp)
    val trigAmt:List[Double] = calls.map(_.bonusAmount.toDouble)
    val forwardStrikes: List[Option[UnderlyingFixing]] = calls.map(_.optionalForwardStrikes)
    val targets:List[Option[Double]] = calls.map(_.targetRedemptionCondition.target.collect{case v => v.toDouble})
    val removeSatisfiedTriggers: List[Boolean] = calls.map(_.triggerCondition.removeSatisfiedTriggers)

    priceTrig(
			paymentList = payoffs,
			fixingList = fixingList,
			callFixingList = callFixingList,
			acc = List.empty,
			triggerStrikes = triggerStrikes,
			trigUp = trigUp,
			trigamt = trigAmt,
			forwardStrikes = forwardStrikes,
			targets = targets,
			removeSatisfiedTriggers = removeSatisfiedTriggers,
			dayCounts = dayCounts,
			accruedPayment = accruedPayment.getOrElse(0.0),
			triggered = false
		)
  }

  /*
 * Returns price array, to be used when there's more than one fixing dates per payoff and with trigger.
 * @param fixings market parameters as Map(variable name -> value) in order of payoff.
 */
  def price(
    fixingList:List[List[UnderlyingFixing]],
		callFixingList:List[Option[UnderlyingFixing]],
		triggerStrikes:List[Option[UnderlyingFixing]],
    trigUp: List[Boolean],
    trigAmount:List[Double],
    forwardStrikes: List[Option[UnderlyingFixing]],
    targets:List[Option[Double]],
    removeSatisfiedTriggers:List[Boolean],
    dayCounts:List[Double],
    accruedPayment:Option[Double]
  )(implicit d1:DI, d2:DI, d3:DI):List[Double] = {
    assert(fixingList.size == payoffs.size && fixingList.size == triggerStrikes.size, s"Number of fixings(${fixingList.size}), trigger(${triggerStrikes.size}) and payoffs(${payoffs.size}) must match - fixings:${fixingList} triggers:${triggerStrikes}")
    priceTrig(
			paymentList = payoffs,
			fixingList = fixingList,
			callFixingList = callFixingList,
			acc = List.empty,
			triggerStrikes = triggerStrikes,
			trigUp = trigUp,
			trigamt = trigAmount,
			forwardStrikes = forwardStrikes,
			targets = targets,
			removeSatisfiedTriggers = removeSatisfiedTriggers,
			dayCounts = dayCounts,
			accruedPayment = accruedPayment.getOrElse(0.0),
			triggered = false
		)
  }

  @tailrec
  private def priceTrig(
    paymentList:List[Payoff],
    fixingList:List[List[UnderlyingFixing]],
		callFixingList:List[Option[UnderlyingFixing]],
    acc:List[Double],
    triggerStrikes:List[Option[UnderlyingFixing]],
    trigUp:List[Boolean],
    trigamt:List[Double],
    forwardStrikes: List[Option[UnderlyingFixing]],
    targets:List[Option[Double]],
    removeSatisfiedTriggers:List[Boolean],
    dayCounts:List[Double],
    accruedPayment:Double,
    triggered:Boolean
  ):List[Double] = (paymentList, fixingList, callFixingList, triggerStrikes, trigUp, trigamt, forwardStrikes, targets, removeSatisfiedTriggers, dayCounts) match {

    case (Nil, _, _, _, _, _, _, _, _, _) => acc.reverse

    case _ if triggered => acc.reverse ++ List.fill(paymentList.tail.size)(0.0)

    case (ph::pt, fh::ft, cfh::cft, tlh::tlt, tuh::tut, tah::tat, fsh::fst, tgth::tgtt, memh::memt, dh::dt) =>
      val eventDateFixing = ph.getEventDateFixing(fh).getOrElse(UnderlyingFixing.empty)
      val couponRate = ph.price(fh, acc)

      if ((tlh, cfh) match {
				case (Some(trig), Some(callFixing)) => ph.isTriggered(callFixing, trig, tuh)
				case _ => false
			}) {
        val trigAmount = (tah + 1.0) / dh
        ((couponRate + ph.terminationAmount(eventDateFixing, trigAmount, fsh)) :: acc).reverse ++ List.fill(pt.size)(0.0)
      }

      else {
        val paidAmount = accruedPayment + couponRate * dh
        tgth match {
          case Some(tgt) if paidAmount >= tgt => //((couponRate + trigamt.head)::acc).reverse ++ List.fill(paymentList.tail.size)(0.0)
            val trigAmount = (tah + 1.0) / dh
            ((couponRate + ph.terminationAmount(eventDateFixing, trigAmount, fsh))::acc).reverse ++ List.fill(pt.size)(0.0)
          case _ =>
            val remainTrigger = if (memh) {
              tlh.collect { case trig =>
                val triggeredUls = ph.triggeredUnderlyings(eventDateFixing, trig, tuh)
                if (triggeredUls.isEmpty) tlt
                else tlt.map(tt => tt.collect { case fut =>
									UnderlyingFixing(fut.getDecimal.filter{ case (kk, vv) => !triggeredUls.contains(kk)})
								})
              }.getOrElse(tlt)
            } else tlt

            priceTrig(pt, ft, cft, couponRate :: acc, remainTrigger, tut, tat, fst, tgtt, memt, dt, paidAmount, false)
        }
      }
    case _ => acc.reverse ++ List.fill(paymentList.tail.size)(Double.NaN)
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
	
	def apply(
		formula:String,
		legs:Int = 1
	)(implicit fixingInfo:FixingInformation):Option[Payoffs] =	{
	  if (legs == 0) Some(Payoffs(List.empty))
	  else if (formula == null || formula.trim.isEmpty) {
	    def getNullPayoff = new NullPayoff()
	    Some(Payoffs(List.fill(legs)(getNullPayoff)))
	  }
	  else {
	    val payofflist:List[Payoff] = formula.jsonNode match {
	      case Some(n) if n.isArray && n.size > 0 => n.elements.asScala.toList.map(f => getPayoff(toJsonString(f)))
	      case _ => formula.split(";").toList.map(getPayoff)
	    }
	    
	    def getFirstElement:Payoff = formula.jsonNode match {
	      case Some(n) if n.isArray && n.size > 0 => getPayoff(toJsonString(n.elements.asScala.toList.head))
	      case _ => getPayoff(formula.split(";").head)
	    }
	  
	  	val fullpayoff = {
				if (payofflist.size < legs) List.fill(legs - payofflist.size)(getFirstElement) ++ payofflist
				else if (payofflist.size > legs) payofflist.take(legs)
				else payofflist
			}
	  	Some(Payoffs(fullpayoff))
	}}

	def toJsonString(n:JsonNode):String = JsonUtils.jsonString(n) //(new ObjectMapper).writeValueAsString(n)
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case _ => formula.parseJsonString("type").orNull
	}
	
	def getPayoff(f:String)(implicit fixingInfo:FixingInformation):Payoff = Payoff(f).getOrElse(GeneralPayoff(f))
	  
}
