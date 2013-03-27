package squantlib.util

import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}
import squantlib.util.DisplayUtils._


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
  	  variables.foreach{ case (k, v) => cf = cf.replace(k, v.toString)}
  	  parse(cf)
  	}
	  
    def parse(cashflow:String):(Map[Set[String], Double], Option[Double], Option[Double]) = {
	  if (cashflow == null || cashflow.isEmpty) return (Map.empty, None, None)
      
      var coeff:mutableMap[Set[String], Double] = mutableMap.empty
      var cap:Option[Double] = None
      var floor:Option[Double] = None
      
      var rateformula = (if(cashflow.trim.head == '-') "0" else "") + replacestr(cashflow, 
          Map(" " -> "", "\r" -> "", "\n" -> "", "-" -> "+-1*", ">" -> "+>", "<" -> "+<"))

      rateformula.split('+').withFilter(!_.isEmpty).foreach{ 
          case s if (s.head == '<') => {
	        cap = s.tail.parseDouble.collect {
	            case v => cap match {
	              case None => v
	              case Some(c) => c.min(v)
	      }}}
          
          case s if (s.head == '>') => {
            floor = s.tail.parseDouble.collect {
              case v => floor match {
	          	case None => v
	          	case Some(f) => f.max(v)
	      }}}
          
          case s => {
            val splitmult = s.replace("/", "*/").split('*').filter(!_.isEmpty)
		    var c = 1.00
		    var varnames = Set.empty[String]
		    
		    splitmult.foreach{ 
		        case t if (t.head == '/') => {
		          val valuepart = t.tail
		          valuepart.parseDouble match {
		            case Some(v) => { c /= v }
		            case None => {
		              if (isFX(valuepart)) varnames += flipFX(valuepart) 
		              else varnames += t
		        }}}
		        
		        case t => {
		          t.parseDouble match {
		            case Some(v) => c = c * v
		            case None => varnames += t
		        }}
		      }
		        
		      if (coeff.contains(varnames)) coeff(varnames) += c
		      else coeff.update(varnames, c)
	        }
	     }

      (Map(coeff.toSeq:_*), floor, cap)
    }
  	
    def calculate(cashflow:String):Option[Double] = {
	  if (cashflow == null || cashflow.isEmpty) return None
      
      var cap:Option[Double] = None
      var floor:Option[Double] = None
      var total:Double = 0.0
      
      var rateformula = (if(cashflow.trim.head == '-') "0" else "") + replacestr(cashflow, 
          Map(" " -> "", "\r" -> "", "\n" -> "", "-" -> "+-1*", ">" -> "+>", "<" -> "+<"))

      rateformula.split('+').withFilter(!_.isEmpty).foreach{ 
          case s if (s.head == '<') => {
	        cap = s.tail.parseDouble.collect {
	            case v => cap match {
	              case None => v
	              case Some(c) => c.min(v)
	      }}}
          
          case s if (s.head == '>') => {
            floor = s.tail.parseDouble.collect {
              case v => floor match {
	          	case None => v
	          	case Some(f) => f.max(v)
	      }}}
          
          case s => {
            val splitmult = s.replace("/", "*/").split('*').filter(!_.isEmpty)
		    var c = 1.00
		    var varnames = Set.empty[String]
		    
		    splitmult.foreach{ 
		        case t if (t.head == '/') => {
		          val valuepart = t.tail
		          valuepart.parseDouble match {
		            case Some(v) => { c /= v }
		            case None => {
		              return None
		        }}}
		        
		        case t => {
		          t.parseDouble match {
		            case Some(v) => c = c * v
		            case None => return None
		        }}
		      }
		        
		      total = total + c
	        }
	     }
	     
	  	 if (cap.isDefined) total = math.min(total, cap.get)
	  	 if (floor.isDefined) total = math.max(total, floor.get)
	  	 
	     Some(total)

    }
  	
    
	
	def isFX(s:String):Boolean = 
	  (s != null && s.size == 6 && Currencies.contains(s.substring(0, 3)) && Currencies.contains(s.substring(3, 6)))
	  
	def flipFX(s:String):String = if (isFX(s)) s.substring(3, 6) + s.substring(0, 3) else null

	def replacestr(s:String, replacef:Map[String, String]):String = {
	  var resultstring = s
	  replacef.keySet.foreach{r => {
	    resultstring = resultstring.replace(r, replacef(r))
	  }}
	  resultstring
	}
}
