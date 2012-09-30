package squantlib.math

import squantlib.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}

class Payoff(val formula:String){
  
	val (parsedFormula:Map[Set[String], Double], floor:Option[Double], cap:Option[Double]) = Payoff.split(formula)
	
	val variables:Set[String] = parsedFormula.keySet.flatten
	 
	def leverage(index:String):Double = parsedFormula.getOrElse(Set(index), 0.0)
	def leverage(index:Set[String]):Double = parsedFormula.getOrElse(index, 0.0)
	
	def constant:Double = parsedFormula.getOrElse(Set.empty, 0.0)
	
	def price(fixings:Map[String, Double]):Double = 
	  if (!variables.forall(x => fixings.keySet.contains(x))) return Double.NaN
	  else {
	    var rate = parsedFormula.map{case (vs, c) => {
	      (if (vs.isEmpty) 1.0 else vs.toList.map(x => fixings(x)).product) * c}}.sum
	    if (floor.isDefined) rate = rate.max(floor.get)
		if (cap.isDefined) rate = rate.min(cap.get)
		rate
		}
	
	override def toString:String = parsedFormula.map{case (vs, c) =>
	   (if (vs.isEmpty) "Const" else vs.reduce((x, y) => x + ", " + y)) + ", " + c
	}.reduce((x, y) => x + "\n" + y)
}

object Payoff {
  
    def variables(cashflow:String):Set[String] = {
      if (cashflow == null) return Set()
      
      var rateformula = replacestr(cashflow, Map(" " -> "", "%" -> "", "\r" -> "", "\n" -> "", "-" -> "+", "*" -> "+", "/" -> "+/"))
      val splitsum = rateformula.split('+')
      
      splitsum.map( s => {
        if (s.head == '/') (if (isFX(s.tail)) flipFX(s.tail) else s.tail)
        else s
      }).filter(s => parseDouble(s).isEmpty).toSet
    }
  
    def split(cashflow:String):(Map[Set[String], Double], Option[Double], Option[Double]) = {
      if (cashflow.isEmpty) return (Map.empty, None, None)
      
      var coeff:mutableMap[Set[String], Double] = mutableMap.empty
      var cap:Option[Double] = None
      var floor:Option[Double] = None
      
      var rateformula = (if(cashflow.trim.head == '-') "0" else "") + replacestr(cashflow, 
          Map(" " -> "", "%" -> "", "\r" -> "", "\n" -> "", "-" -> "+-1*", ">" -> "+>", "<" -> "+<"))

      val splitsum:Array[String] = rateformula.split('+').filter(!_.isEmpty)
        
      splitsum.foreach( s => {
        if (s.head == '<') {
        	val outvalue = parseDouble(s.tail)
            if (cap.isEmpty) cap = outvalue.collect{case v => v/100}
            else cap = outvalue.collect{case v => cap.get.min(v/100)}
        }
          
        else if (s.head == '>') {
        	val outvalue = parseDouble(s.tail)
            if (floor.isEmpty) floor = outvalue.collect{case v => v/100}
            else floor = outvalue.collect{case v => floor.get.max(v/100)}
        }
        
        else {
	        val splitmult:Array[String] = s.replace("/", "*/").split('*').filter(!_.isEmpty)
	        var c = 1.00
	        var varnames:Set[String] = Set()
	        splitmult.foreach( t => {
	          if (t.head == '/') {
	        	val outvalue = parseDouble(t.tail)
	            if (outvalue.isDefined) c /= outvalue.get
	            else if (isFX(t.tail)) varnames += flipFX(t.tail)
	            else varnames += t
	          }
	          
	          else{
	        	val outvalue = parseDouble(t)
	            if (outvalue.isDefined) c = c * outvalue.get
	            else varnames += t
	          }
	        })
	        
	        if (varnames.isEmpty) c /= 100
	        if (coeff.contains(varnames)) coeff(varnames) += c
	        else coeff.update(varnames, c)
        }
      })

      (Map(coeff.toSeq:_*), floor, cap)
    }
    
	def parseDouble(s: String):Option[Double] = try { Some(s.toDouble) } catch { case _ => None }
	
	def isFX(s:String):Boolean = 
	  (s != null && s.length == 6 && Currencies.contains(s.substring(0, 3)) && Currencies.contains(s.substring(3, 6)))
	  
	def flipFX(s:String):String = s.substring(3, 6) + s.substring(0, 3)

	def replacestr(s:String, replacef:Map[String, String]):String = {
	  var resultstring = s
	  replacef.keySet.foreach(r => {
	    resultstring = resultstring.replace(r, replacef(r))
	  })
	  resultstring
	}
}

