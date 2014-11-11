package net.squantlib.schedule.payoff

import net.squantlib.util.FixingInformation

object PayoffExamples {
  
  implicit val fixingInfo = FixingInformation(None, None, None, Map.empty)
  
	val fixed = Payoff("0.5%")
	
	val fixed_json = Payoff("""
	    {"type" : "fixed",
	    "payoff" : "0.5%",
	    "description" : "0.5% fixed coupon"
	    }""")
	    
	
	val linear1d = Payoff("12.5% * usdjpy / 80.2 - 10% > 0.5% < 3%")
	
	val linear1d_json = Payoff("""
	    {"type" : "linear1d", 
	    "variable" : "usdjpy", 
	    "payoff" : 
			{"min" : "0.5%", 
			"max" : "3.00%", 
			"mult" : 0.00155860349127182, 
			"add" : "-10%", 
			"description" : "PRDC: 12.5% * FX/80.2 - 10%"}
	    }""")
	
	    
	val leps1d = Payoff("leps -12.5% * usdjpy / 80.2 - 10% > 68 < 80, 200% * usdjpy / 80.2 - 100% > 75 < 95")
	
	val leps1d_json = Payoff("""
	    {"type" : "leps1d", 
	    "variable" : "usdjpy", 
	    "description" : "Double PRDC formula", 
	    "payoff" : [
			{"minrange" : 60, "maxrange" : 80, "mult" : -0.00155860349127182, "add" : "-10%"}, 
			{"minrange" : 75, "maxrange" : 95, "mult" : 0.0249376558603491, "add" : "-100%"}]
	    }""")

	    
	 val fixseries = Payoffs("0.005; 0.6%; 0.7%; 0.008")
	 
	 
	 val linear1dseries = Payoffs("12.5% * usdjpy/80.2 - 10% > 0.5% < 3%; 20% * usdjpy/80.2 - 10% > 0% <10%")
	 
	  val leps1dseries = Payoffs("leps -12.5% * usdjpy/80.2 - 10% > 68 < 80, 200% * usdjpy / 80.2 - 100% > 75 < 95;  leps 12.5% * usdjpy / 80.2 - 10% > 68 < 80, -200% * usdjpy / 80.2 - 100% > 75 < 95")

	  val fixedleg = Payoff("0.5%").orNull
	  val linear1dleg = Payoff("12.5% * usdjpy / 80.2 - 10% > 0.5% < 3%").orNull
      val combileg = Payoffs(List(fixedleg, linear1dleg))
      
      val binary = Payoff("""
	    {"type" : "binary", 
	    "variable" : ["usdjpy", "NKY"], 
	    "description" : "Binary Payoff", 
	    "payoff" : [
			{"amount" : "0.01%"}, 
			{"strike" : [60, 7000], "amount" : "1.5%"}, 
			{"strike" : [75, 10000], "amount" : "3%"}, 
			{"strike" : [100, 5000], "amount" : "10%"}
	    ]}""")
      
      val putdi = Payoff("""
	    {"type" : "putdi", 
	    "variable" : ["usdjpy", "NKY"], 
	    "trigger" : [70, 7000], 
	    "strike" : [100, 10000], 
	    "description" : "Put Down&In"
	    }""")

      val forward = Payoff("""
	    {"type" : "forward", 
	    "variable" : ["usdjpy", "NKY"], 
	    "strike" : [100, 10000], 
	    "description" : "Forward"
	    }""")

      val rangeforward = Payoff("""
        {"type" : "rangeforward",
        "variable" : "usdjpy",
        "triggerhigh" : "100 + ^spread",
        "triggerlow" : "90",
        "strike" : "100",
        "description" : "Range Forward",
        "^spread" : "-10"
        }""")

	  val putdiamerican = Payoff("""
	      {type:"putdiamerican", 
	      variable:["usdjpy", "NKY"], 
	      trigger:[70, 7000], 
	      strike:[100, 10000], 
	      refstart:"2014/01/01", 
	      refend:"2014/12/31", 
	      description:"Put DI American"
	      }""")
	    
      val general_json = Payoff("""{"type":"general", "payoff":"24.00%*AUDJPY/strike-20.00% > 1% < 6%", "strike":"80.2"}""")
      val general = Payoff("""24.00%*AUDJPY/80.2-20.00% > 1% < 6%""")
      
}


