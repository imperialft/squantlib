package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate}

case class ScheduledPayoffs(schedule:Schedule, payoffs:Payoffs) extends LinearSeq[(CalculationPeriod, Payoff)]{
  
  lazy val scheduledPayoffs = (schedule zip payoffs)
  
  val eventDateLegs:List[List[qlDate]] = scheduledPayoffs.map{case (d, p) => p.eventDates(d)}.toList
  
  val eventDate:List[qlDate] = eventDateLegs.flatten.toSet.toList.sorted
  
  val dateMapper:List[List[Int]] = eventDateLegs.map(_.map(eventDate.indexOf(_)))
  
  def priceMapper[T](fixings:List[T]):List[List[T]] = dateMapper.map(_.map(fixings))
  
  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = {
    val f:List[List[Double]] = priceMapper(fixings)
    payoffs.price(f)
  }

  def price(fixings:List[Map[String, Double]]):List[Double] = 
    payoffs.price(priceMapper(fixings))
    
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, trigAmount)
    
  def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, trigAmount)

  def getAfter(vd:qlDate):ScheduledPayoffs = ScheduledPayoffs(filter{case (cp, _) => cp.paymentDate gt vd})
  
  override def apply(i:Int):(CalculationPeriod, Payoff) = scheduledPayoffs(i)
  
  override def toString = scheduledPayoffs.map{case (d, p) => d.toString + " " + p.toString}.mkString("\n")
	
  override def isEmpty:Boolean = scheduledPayoffs.isEmpty
	
  override def head:(CalculationPeriod, Payoff) = scheduledPayoffs.head
	
  override def tail = scheduledPayoffs.tail
	
  override def length = scheduledPayoffs.length
	
  override def iterator:Iterator[(CalculationPeriod, Payoff)] = scheduledPayoffs.iterator
	
  override def toList:List[(CalculationPeriod, Payoff)] = scheduledPayoffs.toList

}


object ScheduledPayoffs {
  
  def empty:ScheduledPayoffs = ScheduledPayoffs(Schedule.empty, Payoffs.empty)
  
  def apply(payoffshedule:LinearSeq[(CalculationPeriod, Payoff)]):ScheduledPayoffs = 
    payoffshedule.unzip match { case (s, po) => ScheduledPayoffs(Schedule(s), Payoffs(po))}
  
}