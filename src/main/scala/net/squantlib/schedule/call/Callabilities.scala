package net.squantlib.schedule.call

import scala.language.postfixOps
import scala.collection.LinearSeq
//import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.schedule.FixingLegs
import net.squantlib.util.FixingInformation
import net.squantlib.schedule.Schedule
import net.squantlib.schedule.payoff.Payoffs
import org.codehaus.jackson.JsonNode

case class Callabilities(calls:List[Callability]) extends LinearSeq[Callability] with FixingLegs[Callability]{
  
	val underlyings:Set[String] = calls.map(_.underlyings).flatten.toSet
	
	def bermudans:List[Boolean] = calls.map(_.bermudan)
	
	def triggers:List[Option[Map[String, Double]]] = calls.map(c => 
	  if (c.isTriggered) Some(c.triggers.map{case (k, v) => (k, 0.0)})
	  else if (c.isFixed) None
	  else if (c isTrigger) Some(c.triggers) 
	  else None
  )

  def forwardStrikes:List[Option[Map[String, Double]]] = calls.map(c => 
    if (c.forward.isEmpty) None
    else Some(c.forward)
  )
  
  def triggerUps:List[Boolean] = calls.map(c => c.triggerUp).toList
	  
	def triggerValues(variables:List[String]):List[List[Option[Double]]] = calls.map(_.triggerValues(variables))
	
	def isTargetRedemption = calls.exists(c => c.targetRedemption.isDefined)

	def targetRedemptions:List[Option[Double]] = calls.map(_.targetRedemption)
	
	def isTriggeredByTrigger:Boolean = calls.exists(c => c.isFixed && c.fixedTriggerByTrigger == Some(true))

  def isTriggeredByTarget:Boolean = calls.exists(c => c.fixedTriggerByTargetRedemption == Some(true))

	def isTriggered:Boolean = calls.exists(c => c.isFixed && c.fixedTrigger == Some(true))
  
	val isPriceable:Boolean = calls.forall(_.isPriceable)
	
	val bonusAmount:List[Double] = calls.map(_.bonusAmount)
	
	def fill(legs:Int) = size match {
	  case l if l > legs => Callabilities(this.takeRight(legs))
	  case l if l == legs => Callabilities(this)
	  case l if l < legs => Callabilities(List.fill(legs - l)(Callability.empty) ++ this)
	}
	
	def ++(another:Callabilities) = new Callabilities(calls ++ another.calls)
	
	def :+(call:Callability) = new Callabilities(calls :+ call)
	
	override def toString = calls.map(_.toString).mkString("\n")
	
  def apply(i:Int):Callability = calls(i)
    
  def isBermuda:Boolean = calls.exists(_.bermudan)
    
  val isTrigger = calls.exists(_.isTrigger) && !isTriggeredByTrigger
    
	def triggerCheck(fixings:List[Map[String, Double]]):List[Boolean] = (fixings, calls).zipped.map{case (f, c) => c.isTriggered(f)}
	
	override def isEmpty:Boolean = calls.isEmpty || calls.forall(_.isEmpty)
	
	override def head:Callability = calls.head
	
	override def tail = calls.tail
	
	override def length = calls.length
	
	override def iterator:Iterator[Callability] = calls.iterator
	
	override def toList:List[Callability] = calls
	
	def reorder(order:List[Int]) = new Callabilities((0 to calls.size-1).map(i => calls(order(i))) (collection.breakOut)) 
	
	override def isFixed:Boolean = fixinglegs.forall(_.isFixed) || isTriggered
	
	override val fixinglegs = calls
	
	def assignAccumulatedPayments(schedule:Schedule, payoffs:Payoffs) = {
    val payments = (schedule, payoffs).zipped.map{case (s, p) => (s.paymentDate, if (p.price.isNaN) 0.0 else p.price * s.dayCount)}
    (schedule, calls).zipped.foreach{case (s, c) => 
      c.accumulatedPayments = Some(payments.filter{case (d, p) => d <= s.paymentDate}.map{case (d, p) => p}.sum)
    }
	}
	
}


object Callabilities {
	
	def empty:Callabilities = new Callabilities(List.empty)
	
	def apply(calls:LinearSeq[Callability]):Callabilities = new Callabilities(calls.toList)
	
	def bermudan(formula:String, legs:Int):List[Boolean] = bermudanList(formula, legs) match {
	  case berms if !berms.isEmpty && berms.takeRight(1).head => berms.dropRight(1) :+ false
	  case berms => berms
	}
	
  def bermudanList(formula:String, nbLegs:Int):List[Boolean] = formula.jsonNode match {
//    case Some(b) if b.isArray && b.size == 1 => List.fill(nbLegs - 2)(b.head.parseString == Some("berm")) ++ List(false, false)
    case Some(b) if b isArray => 
      val bermList = b.asScala.map(_ match {
        case s if s.isTextual => s.parseString == Some("berm")
        case s if s.isObject => s.parseString("type") == Some("berm")
        case _ => false
      }).toList
      
      List.fill(nbLegs - 2 - b.size)(false) ++ bermList ++ List(false, false)

    case _ => List.fill(nbLegs)(false)
  }
  
//  def target(formula:String, legs:Int):List[Option[Double]] = targetList(formula, legs) match {
//    case targets if !targets.isEmpty && targets.takeRight(1).head.isDefined => targets.dropRight(1) :+ None
//    case targets => targets
//  }
  
  def targetList(formula:String, nbLegs:Int):List[Option[Double]] = formula.jsonNode match {
    case Some(bs) if bs isArray => 
      val targetList = bs.asScala.map(_.get("target").parseDouble).toList
      List.fill(nbLegs - 2 - bs.size)(None) ++ targetList ++ List(None, None)

    case _ => List.fill(nbLegs)(None)
  }

  def callOptionList(formula:String, nbLegs:Int)(implicit fixingInfo:FixingInformation):List[CallOption] = formula.jsonNode match {
    case Some(bb) if bb isArray => 
      val optlist = bb.asScala.map{case b =>
        val invertedStrike:Boolean = b.parseInt("inverted_strike").getOrElse(0) == 1
        val invertedTrigger:Boolean = (invertedStrike || b.parseInt("inverted_trigger").getOrElse(0) == 1)
        val invertedForward:Boolean = (invertedStrike || b.parseInt("inverted_forward").getOrElse(0) == 1)

        val triggerUp = b.parseString("trigger_type").getOrElse("up") == "up"
        val forwardMap = b.getOption("forward").collect{case k => k.parseStringFields}.getOrElse(Map.empty)
        val forward = assignFixings(forwardMap)
        val bonus = b.parseDouble("bonus").getOrElse(0.0)
        val removeSatisfiedTriggers = b.parseInt("memory").getOrElse(0) == 1

        CallOption(
          if (invertedTrigger) !triggerUp else triggerUp,
          if (invertedForward && forward.keys.forall(_.size == 6)) forward.map{case (k, v) => ((k takeRight 3) + (k take 3), if(v != 0.0) 1.0 / v else 0.0)}.toMap else forward,
          forwardMap,
          bonus,
          invertedTrigger,
          invertedForward,
          removeSatisfiedTriggers
        )

      }.toList
        
      List.fill(nbLegs - 2 - bb.size)(CallOption.empty) ++ optlist ++ List(CallOption.empty, CallOption.empty)
    
    case _ => List.fill(nbLegs)(CallOption.empty)
  }
  
  private def triggerList(formula:String, nbLegs:Int, underlyings:List[String]):List[Map[String, String]] = formula.jsonNode match {
    case Some(bs) if bs isArray =>
      val trigList:List[Map[String, String]] = bs.asScala.map(_ match {
        case n if (n isArray) => 
          (underlyings, n.asScala.map(_.parseString.getOrElse("")).toList).zipped.toMap

        case n if (n.isObject) => 
          n.getOption("trigger") match {
            case Some(trigs) if trigs.isArray =>
              (underlyings, trigs.asScala.map(_.parseString.getOrElse(""))).zipped.toMap
            case Some(trigs) if trigs.isObject => 
              trigs.parseStringFields
            case _ => Map.empty[String, String]
          }
          
        case _ => 
          Map.empty[String, String]
      }).toList

      List.fill(nbLegs - bs.size - 2)(Map.empty[String, String]) ++ trigList ++ List.fill(2)(Map.empty[String, String])

    case _ => List.fill(nbLegs)(Map.empty)
  }


  private def triggerToAssignedTrigger(trigs:List[Map[String, String]], invertedStrikes:List[Boolean])(implicit fixingInfo:FixingInformation):List[Map[String, Double]] =
    (trigs, invertedStrikes).zipped.map{
      case (trig, inverted) if inverted =>
        val assignedTrig:Map[String, Double] = assignFixings(trig)
        if (inverted && trig.keys.forall(_.size == 6)) {
            assignedTrig.map{case (k, v) => ((k takeRight 3) + (k take 3), if(v != 0.0) 1.0 / v else 0.0)}.toMap
          } else assignedTrig
      case (trig, inverted) => assignFixings(trig)
    }.toList
  
  private def assignFixings(stks:Map[String, String])(implicit fixingInfo:FixingInformation):Map[String, Double] = {
    //stks.map{case (k, v) => (k, fixingInfo.updateCompute(v))}.collect{case (k, Some(v)) => (k, v)}.toMap
    stks.map{case (k, v) => (k, fixingInfo.updateCompute(v))}.collect{case (k, v) => (k, v.getOrElse(Double.NaN))}.toMap
  }
  
    
	private def triggerMap(formula:List[List[String]], underlyings:List[String])(implicit fixingInfo:FixingInformation):List[Map[String, Double]] = {
	  formula.map(trig => 
	    (underlyings, trig).zipped.map{case (k, v) => (k, fixingInfo.updateCompute(v))}
	    .collect{case (k, Some(v)) => (k, v)}.toMap
	   )
	}

  private def mapList(formula:String, legs:Int):List[Map[String, Any]] = formula.jsonNode match {
    case Some(bs) if bs.isArray =>
      val formulaList:List[Map[String, Any]] = bs.asScala.map(_ match {
        case n if (n.isObject) => n.parseMap
        case _ => Map.empty[String, Any]
      }).toList

      List.fill(legs - bs.size - 2)(Map.empty[String, String]) ++ formulaList ++ List.fill(2)(Map.empty[String, Any])

    case _ => List.fill(legs)(Map.empty)
  }

    
  def apply(
	  formula:String, 
    underlyings:List[String], 
    legs:Int
  )(implicit fixingInfo:FixingInformation):Callabilities = {
    
    val bermudans = bermudan(formula, legs)

    val trigFormulas:List[Map[String, String]] = triggerList(formula, legs, underlyings)

    val callOptions:List[CallOption] = callOptionList(formula, legs)

    val trigMap:List[Map[String, Double]] = triggerToAssignedTrigger(trigFormulas, callOptions.map(c => c.invertedTrigger))
    
    val targets:List[Option[Double]] = targetList(formula, legs)

    val baseFormulas:List[Map[String, Any]] = mapList(formula, legs)


    val calls = (bermudans.zip(trigFormulas)).zip(trigMap.zip(targets)).zip(callOptions.zip(baseFormulas)).map{
      case (((berm, f), (trig, tgt)), (callOption, baseFormula)) =>

        var inputString:Map[String, Any] = baseFormula

        inputString = inputString.updated("type", (
          if (berm) "berm"
          else if (tgt.isDefined) "target"
          else if (!f.isEmpty) "trigger"
          else baseFormula.get("type") match {
            case Some(v) => v
            case _ => "unknown"
          }
        ))

        if (tgt.isDefined) {
          inputString = inputString.updated("target", tgt.getOrElse(Double.NaN))
        } else if (!f.isEmpty) {
          inputString = inputString.updated("trigger", f).updated("trigger_type", (if(callOption.triggerUp) "up" else "down"))
        }
  
        if (!callOption.forward.isEmpty) {
          inputString = inputString.updated("forward", callOption.forwardInputString)
        }
        
        if (callOption.invertedTrigger) {
          inputString = inputString.updated("inverted_trigger", 1)
        }

        if (callOption.invertedForward) {
          inputString = inputString.updated("inverted_forward", 1)
        }

        inputString = inputString.updated("bonus", callOption.bonus)
          
        Callability(
          bermudan = berm,
          triggers = trig,
          targetRedemption = tgt,
          callOption = callOption,
          inputString = inputString,
          accumulatedPayments = None,
          simulatedFrontier= Map.empty
        )
    }
    
    Callabilities(calls)
  }
	  
	
	def apply(
    bermudans:List[Boolean], 
    triggers:List[List[Option[Double]]], 
    targets:List[Option[Double]],
    underlyings:List[String], 
    callOptions:List[CallOption])(implicit fixingInfo:FixingInformation):Callabilities = {

    Callabilities(
      bermudans.zip(triggerMap(underlyings, triggers)).zip(callOptions.zip(targets)).map{case ((berm, trig), (callOption, tgt)) => 
        Callability(
          bermudan = berm,
          triggers = trig,
          targetRedemption = tgt,
          callOption = callOption,
          inputString = Map.empty,
          accumulatedPayments = None,
          simulatedFrontier= Map.empty
        )
      }
    )
	}
	  
//	def apply(
//    bermudans:List[Boolean], 
//    triggers:List[List[Option[Double]]], 
//    targets:List[Option[Double]],
//    underlyings:List[String])(implicit fixingInfo:FixingInformation):Callabilities = {
//	  
//    Callabilities(
//      (bermudans, triggerMap(underlyings, triggers), targets).zipped.map{case (berm, trig, tgt) => 
//        Callability(
//          bermudan = berm,
//          triggers = trig,
//          triggerUp = true,
//          targetRedemption = tgt,
//          forward = Map.empty,
//          bonus = 0.0,
//          inputString = Map.empty,
//          accumulatedPayments = None,
//          simulatedFrontier= Map.empty
//        )
//      }
//    )
//  }
	
	def triggerMap(underlyings:List[String], triggers:List[List[Option[Double]]]):List[Map[String, Double]] = {
	  triggers.map(trigs => {
      val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(collection.breakOut)
      t
    })
	}

}

