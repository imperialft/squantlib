package squantlib.util

object mctest {

  def ratedom(t:Double):Double = 0.01 + t / 100.0
  
  def fwdratedom(t1:Double, t2:Double):Double = (ratedom(t2) * t2 - ratedom(t1) * t1) / (t2 - t1)
  
  def ratefor(t:Double):Double = 0.03 + t / 100.0
  
  def fwdratefor(t1:Double, t2:Double):Double = (ratefor(t2) * t2 - ratefor(t1) * t1) / (t2 - t1)
  
  def vol(t:Double):Double = 0.1 + t / 10.0
  
  def fwdvol(t1:Double, t2:Double):Double = vol(t1) // to be reviewed
  
  val spot:Double = 100.0
  
  def random = math.random
  
  def invnorm = invnormfunc(random)
 
  def invnormfunc(n:Double) = n // to be reviewed
  
  val time = List(0.5, 1.0, 1.5, 2)
  
  (1 to 10).foreach{i => 
	  var s = spot
	  var prevt = 0.0
	  
	  time.foreach(t => {
	    val rt = fwdratedom(prevt, t)
	    val qt = fwdratefor(prevt, t)
	    val vol = fwdvol(prevt, t)
	    val dt = t - prevt
	    val dW = invnorm
	    s = s * math.exp((rt - qt - 0.5 * vol * vol) * dt + vol * math.sqrt(dt) * dW)
	    println(t + "\t" + s)
	    prevt = t
	   }
	  )
  }
  
  
  
  
}