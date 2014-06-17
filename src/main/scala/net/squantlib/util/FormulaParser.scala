package net.squantlib.util

import net.squantlib.util.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}
import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec

object FormulaParser {

  def parseList(cashflow:String):List[(Map[Set[String], Double], Option[Double], Option[Double])] = 
    cashflow.split(",").map(parse) (collection.breakOut)
  	
  /** 
   * Parse a linear formula string into sum of named variables
   * returns tuple containing
   * 1) Cashflow as map(variable names -> leverage)
   * 2) Floor (ie. minimum amount)
   * 3) Cap (ie. maximum amount)
   */
	  
  def parse(cashflow:String, variables:Map[String, Double]):(Map[Set[String], Double], Option[Double], Option[Double]) = {
    var cf = cashflow
  	variables.foreach{case (k, v) => cf = cf.replace(k, v.toString)}
  	parse(cf)
  }
  
  def parseNoMerge(cashflow:String):(List[(Set[String], Double)], Option[Double], Option[Double]) = {
    if (cashflow == null || cashflow.isEmpty) return (List.empty, None, None)
    parseRec(breakdownToSums(cashflow), List.empty, None, None)
  }  
  
  def parse(cashflow:String):(Map[Set[String], Double], Option[Double], Option[Double]) = {
    val (p, f, c) = parseNoMerge(cashflow)
    (mergeComponentRec(p, Map.empty), f, c)
  }  
  
  def calculate(cashflow:String):Option[Double] = {
    if (cashflow == null || cashflow.isEmpty) return None
    calculateRec(breakdownToSums(cashflow), 0, None, None)
  }  
	  
  def breakdownToSums(formula:String):List[String] = {
    var rateformula = (if(formula.trim.head == '-') "0" else "") + replacestr(formula, replacedSymbols)
    rateformula.split('+').filter(!_.isEmpty).toList
  }
  
  def breakdownToMultiples(formula:String):List[String] = formula.replace("/", "*/").split('*').filter(!_.isEmpty).toList
  
  @tailrec private def mergeComponentRec(elist:List[(Set[String], Double)], emap:Map[Set[String], Double]):Map[Set[String], Double] = elist match {
    case Nil => emap
    case (s, v)::ss if emap contains s => mergeComponentRec(ss, emap.updated(s, emap(s) + v))
    case (s, v)::ss => mergeComponentRec(ss, emap + (s -> v))
  }
  
  @tailrec private def parseRec(cashflow:List[String], coeff:List[(Set[String], Double)], floor:Option[Double], cap:Option[Double]):(List[(Set[String], Double)], Option[Double], Option[Double]) = cashflow match {
    case Nil => (coeff.reverse.filter{case (s, c) => c != 0.0}, floor, cap)
    case s::ss if (s.head == '<') => 
      val newcap = s.tail.parseDouble.collect{case v => cap match {case None => v; case Some(c) => c.min(v)}}
      parseRec(ss, coeff, floor, newcap)
    case s::ss if (s.head == '>') => 
      val newfloor = s.tail.parseDouble.collect{ case v => floor match {case None => v; case Some(f) => f.max(v)}}
      parseRec(ss, coeff, newfloor, cap)
    case s::ss =>
      parseRec(ss, parseMultRec(breakdownToMultiples(s), Set.empty, 1.0) :: coeff, floor, cap)
  }
  
  @tailrec private def parseMultRec(elems:List[String], vars:Set[String], coeff:Double):(Set[String], Double) = elems match {
    case Nil => (vars, coeff)
    case t::ts if t.head == '/' => 
      val valuepart = t.tail
      valuepart.parseDouble match {
        case Some(v) => parseMultRec(ts, vars, coeff / v)
        case None if isFX(valuepart) => parseMultRec(ts, vars + flipFX(valuepart), coeff)
        case None => parseMultRec(ts, vars + t, coeff)
      }
    case t::ts => 
      t.parseDouble match {
        case Some(v) => parseMultRec(ts, vars, coeff * v)
        case None => parseMultRec(ts, vars + t, coeff)
      }
  }
  
  @tailrec private def calculateRec(cashflow:List[String], result:Double, floor:Option[Double], cap:Option[Double]):Option[Double] = cashflow match {
    case Nil if result.isNaN || result.isInfinity => None
    case Nil => 
      val capped = cap match {case Some(c) => result.min(c); case None => result}
      floor match {case Some(f) => Some(capped.max(f)); case None => Some(capped)}
    case s::ss if (s.head == '<') => 
      val newcap = s.tail.parseDouble.collect{case v => cap match {case None => v; case Some(c) => c.min(v)}}
      calculateRec(ss, result, floor, newcap)
    case s::ss if (s.head == '>') => 
      val newfloor = s.tail.parseDouble.collect{ case v => floor match {case None => v; case Some(f) => f.max(v)}}
      calculateRec(ss, result, newfloor, cap)
    case s::ss =>
      calculateMultRec(breakdownToMultiples(s), 1.0) match {
        case Some(rr) => calculateRec(ss, rr + result, floor, cap)
        case None => None
      }
  }
  
  @tailrec private def calculateMultRec(elems:List[String], result:Double):Option[Double] = elems match {
    case Nil => Some(result)
    case t::ts if t.head == '/' => t.tail.parseDouble match {
      case Some(v) => calculateMultRec(ts, result/v)
      case None => None
    }
    case t::ts => t.parseDouble match {
      case Some(v) => calculateMultRec(ts, result * v)
      case None => None
    }
  }  
  
  private val replacedSymbols = Map(
      " " -> "", 
      "\r" -> "", 
      "\n" -> "", 
      "-" -> "+-1*", 
      ">" -> "+>", 
      "<" -> "+<")
  
	
  def isFX(s:String):Boolean = 
    s != null && 
    s.size == 6 && 
    Currencies.contains(s.substring(0, 3)) && 
    Currencies.contains(s.substring(3, 6))
	  
  def flipFX(s:String):String = if (isFX(s)) s.substring(3, 6) + s.substring(0, 3) else null

  private def replacestr(s:String, replacef:Map[String, String]):String = {
    var resultstring = s
	replacef.keySet.foreach{r => resultstring = resultstring.replace(r, replacef(r))}
    resultstring
  }
  
}
