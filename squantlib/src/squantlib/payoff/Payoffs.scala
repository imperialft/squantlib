package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate}
import scala.Predef.{DummyImplicit => DI, _}
import com.sun.beans.decoder.FalseElementHandler
import com.sun.beans.decoder.FalseElementHandler

case class Payoffs(payoffs:List[Payoff]) extends LinearSeq[Payoff]{
  
	val underlyings:Set[String] = {
	  @tailrec def variablesRec(paylist:List[Payoff], acc:Set[String]):Set[String] = {
		if (paylist.isEmpty) acc
		else variablesRec(paylist.tail, paylist.head.variables ++ acc)
	  }
	  variablesRec(payoffs, Set.empty)
	}
	
	val factors:Int = underlyings.size
	
	abstract class FixingInterpreter[T, U] {
	  def price(fixing:T, payoff:Payoff):Double
	  def triggered(fixing:T, trigger:Option[U]):Boolean
	  def applyFixing(fixing:T, payoff:Payoff):Payoff}
	
	implicit object DoubleList extends FixingInterpreter[Double, Double] {
	  def price(fixing:Double, payoff:Payoff) = if (fixing.isNaN) payoff.price else payoff.price(fixing)
	  def triggered(fixing:Double, trigger:Option[Double]) = trigger.isDefined && fixing > trigger.get
	  def applyFixing(fixing:Double, payoff:Payoff) = payoff.applyFixing(fixing)}
	
	implicit object MapList extends FixingInterpreter[Map[String, Double], Map[String, Double]] {
	  def price(fixing:Map[String, Double], payoff:Payoff) = if (fixing.isEmpty) payoff.price else payoff.price(fixing)
	  def triggered(fixing:Map[String, Double], trigger:Option[Map[String, Double]]) = trigger match {
	    case None => false
	    case Some(t) if t isEmpty => false
	    case Some(t) => t.forall{case (v, d) => d <= fixing(v)}}
	  def applyFixing(fixing:Map[String, Double], payoff:Payoff) = payoff.applyFixing(fixing)}
	
	implicit object ListDoubleList extends FixingInterpreter[List[Double], Double] {
	  def price(fixing:List[Double], payoff:Payoff) = if (fixing.isEmpty) payoff.price else payoff.price(fixing)
	  def triggered(fixing:List[Double], trigger:Option[Double]) = trigger.isDefined && fixing.last > trigger.get
	  def applyFixing(fixing:List[Double], payoff:Payoff) = payoff.applyFixing(fixing.last)}
	
	implicit object ListMapList extends FixingInterpreter[List[Map[String, Double]], Map[String, Double]] {
	  def price(fixing:List[Map[String, Double]], p:Payoff) = if (fixing.isEmpty) p.price else p.price(fixing)
	  def triggered(fixing:List[Map[String, Double]], trigger:Option[Map[String, Double]]) = trigger match {
	    case None => false
	    case Some(t) if t isEmpty => false
	    case Some(t) => t.forall{case (v, d) => d <= fixing.last(v)}}
	  def applyFixing(fixing:List[Map[String, Double]], payoff:Payoff) = payoff.applyFixing(fixing.last)}
	
	@tailrec private def priceRec[T](paylist:List[Payoff], fixlist:List[T], acc:List[Double])
	(implicit fi:FixingInterpreter[T, _]):List[Double] = {
	  if (paylist.isEmpty) acc
	  else priceRec(paylist.tail, fixlist.tail, acc :+ fi.price(fixlist.head, paylist.head))
	}
	
	@tailrec private def priceTrig[T, U](paylist:List[Payoff], fixlist:List[T], acc:List[Double], triglist:List[Option[U]], trigamt:List[Double], triggered:Boolean)
	(implicit fi:FixingInterpreter[T, U]):List[Double] = {
	  if (paylist.isEmpty) acc
//	  else if (triggered) priceTrig(paylist.tail, fixlist.tail, acc :+ 0.0, triglist.tail, trigamt.tail, true)
	  else if (triggered) acc ++ List.fill(paylist.tail.size)(0.0)
//	  else if (fi.triggered(fixlist.head, triglist.head)) priceTrig(paylist.tail, fixlist.tail, acc :+ (fi.price(fixlist.head, paylist.head) + trigamt.head), triglist.tail, trigamt.tail, true)
	  else if (fi.triggered(fixlist.head, triglist.head)) (acc :+ (fi.price(fixlist.head, paylist.head) + trigamt.head)) ++ List.fill(paylist.tail.size)(0.0)
	  else priceTrig(paylist.tail, fixlist.tail, acc :+ fi.price(fixlist.head, paylist.head), triglist.tail, trigamt.tail, false)
	}
	
	@tailrec private def fixingRec[T](paylist:List[Payoff], fixlist:List[T], acc:List[Payoff])
	(implicit fi:FixingInterpreter[T, _]):List[Payoff] = {
	  if (paylist.isEmpty) acc
	  else fixingRec(paylist.tail, fixlist.tail, acc :+ fi.applyFixing(fixlist.head, paylist.head))
	}
	
	
	/*
	 * Select appropriate pricing functions depending on your needs.
	 * Fixing information are provided as either;
	 * 	format				>1 variables	>1 refdates
	 *  List[Double]		no				no			
	 *  List[Map]			yes				no
 	 *  List[List[Double]]	no				yes
 	 *  List[List[Map]]		yes				yes
 	 * 
	 */
	
	/*
	 * Returns price array, when there's no variable.
	 */
	def price:List[Double] = payoffs.map(_.price)
	
	/*
	 * Returns price array, to be used when there's only one fixing dates per payoff, no trigger and only one variable.
	 * @param fixings market parameter fixing value
	 */
	def price(fixings:List[Double]):List[Double] = {
	  assert(fixings.size == payoffs.size && factors <= 1)
	  priceRec(payoffs, fixings, List.empty)
	}
	
	/*
	 * Returns price array, to be used when there's only one fixing dates per payoff
	 * @param fixings market parameters as Map(variable name -> value) in order of payoff.
	 */
	def price(fixings:List[Map[String, Double]])(implicit d:DI):List[Double] = {
	  assert(fixings.size == payoffs.size)
	  priceRec(payoffs, fixings, List.empty)
	}
	
	/*
	 * Returns price array, to be used when there's only one fixing dates per payoff, no trigger and one or more variables.
	 * @param fixings market parameter fixing value
	 */
	def price(fixings:List[List[Double]])(implicit d:DI, d2:DI):List[Double] = {
	  assert(fixings.size == payoffs.size && factors <= 1)
	  priceRec(payoffs, fixings, List.empty)
	}
	
	/*
	 * Returns price array, to be used when there's more than one fixing dates per payoff
	 * @param fixings market parameters as Map(variable name -> value) in order of event dates, in order of payoff.
	 */
	def price(fixings:List[List[Map[String, Double]]])(implicit d:DI, d2:DI, d3:DI):List[Double] = {
	  assert(fixings.size == payoffs.size)
	  priceRec(payoffs, fixings, List.empty)
	}
	
	/*
	 * Returns price array, to be used when there's only one fixing dates per payoff, with trigger and only one variable.
	 * @param fixings market parameter fixing value
	 */
	def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double]):List[Double] = {
	  assert(fixings.size == payoffs.size && fixings.size == trigger.size)
	  priceTrig(payoffs, fixings, List.empty, trigger, trigAmount, false)
	}
	
	/*
	 * Returns price array, to be used when there's only one fixing dates per payoff and with trigger.
	 * @param fixings market parameters as Map(variable name -> value) in order of payoff.
	 */
	def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double])(implicit d1:DI):List[Double] = {
	  assert(fixings.size == payoffs.size && fixings.size == trigger.size)
	  priceTrig(payoffs, fixings, List.empty, trigger, trigAmount, false)
	}
	
	/*
	 * Returns price array, to be used when there's more than one fixing dates per payoff, with trigger and only one variable.
	 * @param fixings market parameter fixing value
	 */
	def price(fixings:List[List[Double]], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d1:DI, d2:DI):List[Double] = {
	  assert(fixings.size == payoffs.size && fixings.size == trigger.size)
	  priceTrig(payoffs, fixings, List.empty, trigger, trigAmount, false)
	}
	
	
	/*
	 * Returns price array, to be used when there's more than one fixing dates per payoff and with trigger.
	 * @param fixings market parameters as Map(variable name -> value) in order of payoff.
	 */
	def price(fixings:List[List[Map[String, Double]]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double])(implicit d1:DI, d2:DI, d3:DI):List[Double] = {
	  assert(fixings.size == payoffs.size && fixings.size == trigger.size)
	  priceTrig(payoffs, fixings, List.empty, trigger, trigAmount, false)
	}
	
	/* 
	 * Replaces already-fixed payoffs to fixed leg
	 */
	def applyFixing(fixings:List[Map[String, Double]]):Payoffs = {
	  assert(fixings.size == payoffs.size)
	  Payoffs(fixingRec(payoffs, fixings, List.empty))
	}
	
	def applyFixing(fixings:List[Option[Double]]) (implicit d:DI):Payoffs = {
	  if (factors == 1) applyFixing(fixings.map{case None => Map.empty[String, Double] case Some(v) => Map(underlyings.head -> v)})
	  else this
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
	
	def apply(formula:String, legs:Int = 0):Option[Payoffs] =	{
	  if (formula == null || formula.trim.isEmpty) None
	  else {
	    val payofflist:List[Payoff] = formula.jsonNode match {
	      case Some(n) if n isArray => n.getElements.toList.map(f => getPayoff(toJsonString(f))).flatten
	      case _ => formula.split(";").toList.map(getPayoff).flatten
	    }
	  
	  	val fullpayoff = if (payofflist.size < legs) List.fill(legs - payofflist.size)(payofflist.head) ++ payofflist else payofflist
	  	Some(Payoffs(fullpayoff))
	}}
	
	def toJsonString(n:JsonNode):String = (new ObjectMapper).writeValueAsString(n)
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type").orNull
	  }
	
	def getPayoff(f:String):List[Payoff] = payoffType(f) match {
	      case "fixed" => List(FixedPayoff(f))
		  case "fixedseries" => FixedPayoffSeries(f)
		  case "leps1d" => List(LEPS1dPayoff(f))
		  case "leps1dseries" => LEPS1dPayoffSeries(f) 
		  case "linear1d" => List(Linear1dPayoff(f))
		  case "linear1dseries" => Linear1dPayoffSeries(f)
		  case "putdi" => List(PutDIPayoff(f))
		  case "forward" => List(ForwardPayoff(f))
		  case "null" => List(NullPayoff(f))
		  case "binary" => List(BinaryPayoff(f))
		  case "general" => List(GeneralPayoff(f))
		  case _ => List(GeneralPayoff(f))
		}

	  
}
