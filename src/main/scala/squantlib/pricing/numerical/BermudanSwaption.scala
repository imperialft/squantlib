package squantlib.pricing.numerical

import squantlib.math.random.{RandomGenerator, MersenneTwister}
import squantlib.math.statistical.NormSInv
import org.apache.commons.math3.linear._

object BermudanSwaption {
  
  def getRandomGenerator:RandomGenerator = new MersenneTwister(1)
  
  /* Standard Black-Scholes calculation
   * @param spot 	current underlying price
   * @param ratedom	continuous risk-free rate of domestic pricing currency
   * @param ratefor	continuous risk-free rate of foreign currency
   * @param sigma	volatility of the underlying FX
   * @param NormSInv Normal inverse cumulative distribution function
   * @param time	time to maturity in years
   * @param discount cash-flow discount factor ZC such that PV = amount x ZC. None if it's discounted by ratedom.
   * @returns price
   */
  
  class ExtendedMatrix(r:RealMatrix) {
    def apply(a:Int, b:Int) = get(a, b)
    def get(a:Int, b:Int) = r.getEntry(a, b)
    def set(a:Int, b:Int, v:Double) = r.setEntry(a, b, v)
    def setRow(row:Int, v:Double) = r.setRow(row, Array.fill(r.getColumnDimension)(v))
    def setColumn(col:Int, v:Double) = r.setColumn(col, Array.fill(r.getRowDimension)(v))
  }
  
//  implicit def realMatrixToExtended(r:RealMatrix) = new ExtendedMatrix(r)
//  
//   def test = {
//
//	/*This program can work for arbitrary no. of factors. You have to specify no.
//	of factors as well as volatility structure for each factor. The volatility
//	structure can be obtained from principal component analysis of correlation
//	matrix and adjusting to calibrated volatilities as done in excellent paper
//	by Rebonato. See my web page for the references
//	(http://www.geocities.com/anan2999). It does not take correlation
//	structure as input. You can also specify CEV constant alpha for skew.
//	Remember changing this constant changes effective volatility.
//	randn('state',[1541045451;4027226640]) add a good random number seed
//	here if you wish.
//	if you don't matlab will choose its own seed.
//		* 
//	*/
//    val randomGenerator = getRandomGenerator
//    def normSInv(x:Double) = NormSInv(x)
//    def getRandom:Double = normSInv(randomGenerator.sample)
//    
//	
//	val delta = 0.25 //Tenor spacing. usually .25 or .5
//	val P = 5000 // No. of paths, do not try more than 5000 paths unless you are very patient
//	
//	val T_e1 = 6.0 //maturity of underlying swap in years(must be an exact multiple of delta)
//	
//	val T_x1 = 5.75  //last exercise date of the swaption (must be an exact multiple of delta)
//	
//	val T_s1 = 3.0 //lockout date (must be an exact multiple of delta)
//	val T_e = (T_e1/delta + 1).round.toInt
//	val T_x = (T_x1/delta + 1).round.toInt
//	val T_s = (T_s1 / delta + 1).round.toInt
//	val N = T_e
//	val F = 2 // number of factors. If you change this line also change volatility structure appropriately
//	
//	val alpha=1.0 // CEV constant alpha for skew.Remember changing this value changes effective volatility. It is 1.0 for lognormal model.
//	val k = 0.1 // strike, fixed coupon
//	val pr_flag = +1 // payer receiver flag; assumes value of +1 for a payer swaption and a value of -1 for a receiver swaption.
//
//	var n_spot=1
//	
////	def repmat(v:Double, dim1:Int, dim2:Int):Array[Array[Double]] = Array.fill(dim1)(Array.fill(dim2)(v))
//	def repmat(v:Double, dim1:Int, dim2:Int):RealMatrix = new Array2DRowRealMatrix(Array.fill(dim1)(Array.fill(dim2)(v)))
//    
//	def repmatf(v:()=> Double, dim1:Int, dim2:Int):RealMatrix = new Array2DRowRealMatrix(Array.fill(dim1)(Array.fill(dim2)(v())))
//    
//    def randn(dim1:Int, dim2:Int):RealMatrix = repmatf(() => getRandom, dim1, dim2)
//	
////	def randn(dim1:Int, dim2:Int):Array[Array[Double]] = Array.fill(dim1)(Array.fill(dim2)(getRandom))
//	
//	val L = repmat(0.10, P, T_e + 1)
//	
//	var vol = repmat(0, T_e, F)
//	
//	for (n <- 0 to N-1) {
//	  for (f <- 0 to F-1) {
//	    if(f == 1) {
//	      vol.setEntry(n, f, 0.15) //volatility of first factor
//	    }
//	    if(f == 2) {
//	      vol.setEntry(n, f, 0.15 - math.pow(0.009 * n * 0.25, 0.5)) //volatility of second factor
//	    }
//	  }
//	}
//
//	//You can add more volatility factors in the above line but please also change F accordingly
//	// var drift = repmat(0, P, F)
//	
//	val money_market = repmat(1, T_x, P)
//	val swap = repmat(0, T_x, P)
//	val B = repmat(1, P, T_e)
//	
//	for(i <- 0 to P-1) money_market.setEntry(1, i, money_market.getEntry(0, i) * (1 + delta * L(i, 0)))
//	
//	var increment = repmat(0, P, 1)
//	
////	val drift = repmat(0, P, F)
//	
//	for (t <- 1 to T_x - 1){
////	  t
//	  val normal_matrix = randn(P,F)
//	  var drift = repmat(0, P, F)
//	  
//	  for (n <- t - 1 to T_e -1){
//	    increment.setColumn(1, 0.0)
//	// n
//	    for (f <- 0 to F - 1){
//	      for (p <- 0 to P-1) {
//	        drift.setColumn(f, drift(p, f) + delta * vol(n - n_spot + 1, f) * (math.pow(L(p, n), alpha) / (1 + delta * L(p, n))))
//	        
//	        increment.set(p, 0, increment(p, 0) + vol(n - n_spot + 1, f) * math.pow(L(p, n), alpha) / L(p, n) * 
//	         				(normal_matrix(p, f) * math.sqrt(delta) - 0.5 * vol(n-n_spot+1, f) * 
//	         				math.pow(L(p, n), alpha) / L(p, n) * delta + drift(p, f) * delta))
//	      }
//	    }
//	    
//	    for (p <- 0 to P - 1) {
//	      L(p)(n) = L(p)(n) * math.exp(increment(p)(1))
//	      L(p)(n) = math.max(L(p)(n), 0.00001)
//	    }
//	  }
//	  
//	  for (p <- 0 to P - 1) {B(p)(t) = 1.0}
//	
//	  for (n <- t to T_e - 1){
//	    for (p <- 0 to P - 1){
//	      B(p)(n)=B(p)(n-1) / (1 + delta * L(p)(n-1))
//	    }
//	  }
//	
//	  for (p <- 0 to P - 1) {
//	    money_market(t+1)(p) = money_market(t)(p) * (1 + delta * L(p)(n_spot))
//	  }
//	
//	  if(t + 1 >= T_s && t < T_x){
//	    for (n <- t to T_e-1) { //the swap leg is determined one date before the end
//	      for (p <- 0 to P - 1) {
//	        swap(t)(p) = swap(t)(p) + (B(p)(n+1) * (L(p)(n) - k ) * pr_flag * delta)
//	        }
//	    }
//	  }
//	  n_spot = n_spot + 1;
//	}
//	
//	var value = repmat(0, P, 1)
//	val stop_rule = repmati(T_x, P, 1)
//	
//	for (p <- 0 to P - 1) {
//	  value(p)(0) = if (swap(T_x)(p) > 0.0) swap(T_x)(p) else 0.0
//	}
//	
//	var coeff = repmat(0, T_x, 6);
//	for (t <- T_x to T_s-1 by -1) {
//	  var i = 0
//	  var a:Array[Array[Double]] = Array.empty
//	  var y:Array[Double] = Array.empty
//	  for (p <- 0 to P-1){
//	    if (swap(t)(p) > 0.0) {
//	      i = i + 1
//	      val b:Array[Double] = Array(
//	          1.0,
//	          swap(t)(p),
//	          swap(t)(p) * swap(t)(p),
//	          money_market(t)(p),
//	          money_market(t)(p) * money_market(t)(p),
//	          money_market(t)(p) * swap(t)(p))
//	      a = a :+ b
//	      y = y :+ money_market(t)(p) / money_market(stop_rule(p)(0))(p) * value(p)(0)
//	   }
//	 }
//	  
//	  val temp = (a.map(aa => aa* aa).sum) * (a, y).zipped.map(_ * _).sum
//	  for (c <- 0 to 5) {coeff(t, c) = temp(c)}
//	expec_cont_value=repmat(0,[P,1]);
//	exer_value=repmat(0,[P,1]);
//	expec_cont_value(:,1)=(coeff(t,1)+coeff(t,2).*swap(t,:)+
//	coeff(t,3).*swap(t,:)...
//	.*swap(t,:)+coeff(t,4).*money_market(t,:)+
//	coeff(t,5).*money_market(t,:)...
//	.*money_market(t,:)+coeff(t,6).*money_market(t,:).*swap(t,:))';
//	exer_value(swap(t,:)>0,1)=(swap(t,swap(t,:)>0))';
//	value((exer_value(:,1)>expec_cont_value(:,1))&(swap(t,:)>0)',1)...
//	=exer_value((exer_value(:,1)> expec_cont_value(:,1))&(swap(t,:)>0)',1);
//	stop_rule((exer_value(:,1)>expec_cont_value(:,1))&(swap(t,:)>0)',1)=t;
//	end
//	price=0;
//	for p=1:P,
//	price=price+ (value(p,1)/(money_market(stop_rule(p,1),p)))/P;
//	end
//	price
//}
  
}
	
	
	
	
//function []=bermudan_swaption()
//%This program can work for arbitrary no. of factors. You have to specify no.
//%of factors as well as volatility structure for each factor. The volatility
//%structure can be obtained from principal component analysis of correlation
//%matrix and adjusting to calibrated volatilities as done in excellent paper
//%by Rebonato. See my web page for the references
//%(http://www.geocities.com/anan2999). It does not take correlation
//%structure as input. You can also specify CEV constant alpha for skew.
//%Remember changing this constant changes effective volatility.
//%randn('state',[1541045451;4027226640]) % add a good random number seed
//%here if you wish.
//%if you don't matlab will choose its own seed.
//delta=.25; %Tenor spacing. usually .25 or .5
//P=5000; % No. of paths, do not try more than 5000 paths unless you are
//% very patient
//T_e1=6.0; %maturity of underlying swap in years(must be an
//%exact multiple of delta)
//T_x1=5.75; %last exercise date of the swaption (must be an
//%exact multiple of delta)
//T_s1=3.0; %lockout date (must be an exact multiple of delta)‘‘London’’ --- 2006/8/23 --- 22:09 --- page 63 --- #65
//Section 1.13 Bermudan Swaption Pricing in Matlab 63
//T_e=T_e1/delta+1;
//T_x=T_x1/delta+1;
//T_s=T_s1/delta+1;
//N=T_e;
//F=2; % number of factors. If you change this line also change volatility
//% structure appropriately
//alpha=1.0;%CEV constant alpha for skew.Remember changing this value changes
//%effective volatility
//%It is 1.0 for lognormal model.
//k=.1; % strike, fixed coupon
//pr_flag=+1; %payer receiver flag; assumes value of +1 for a payer swaption
//%and a value of -1 for a receiver swaption.
//n_spot=2;
//L=repmat(.10,[P,T_e+1]);
//vol=repmat(0,[T_e,F]);
//for n=1:N,
//for f=1:F,
//if(f==1)
//vol(n,f)=.15; %volatility of first factor
//end
//if(f==2)
//vol(n,f)= (.15-(.009*(n)*.25).ˆ.5); %volatility of second factor
//end
//end
//end
//%You can add more vaolatility factors in the above line but please also
//%change F accordingly
//%drift=repmat(0,[P,F]);
//money_market=repmat(1,[T_x,P]);
//swap=repmat(0,[T_x,P]);
//B=repmat(1,[P,T_e]);
//money_market(2,:)=money_market(1,:).*(1+delta*L(:,1))';
//increment=repmat(0,[P,1]);
//drift=repmat(0,[P,F]);
//for t= 2 : T_x,
//t
//normal_matrix=randn([P,F]);
//drift(:,:)=0;
//for n= t : T_e,
//increment(:,1)=0;
//% n
//for f=1:F,
//drift(:,f)=drift(:,f)+ delta*vol(n-n_spot+1,f).*
//((L(:,n).ˆalpha)./(1+delta.*L(:,n))); %‘‘London’’ --- 2006/8/23 --- 22:09 --- page 64 --- #66
//64 Swaps and Fixed Income Instruments Chapter 1
//increment(:,1)=increment(:,1)+vol(n-n_spot+1,f).*
//(L(:,n).ˆalpha)./L(:,n)...
//.*(normal_matrix(:,f).*sqrt(delta)-.5.*vol(n-n_spot+1,f).*
//(L(:,n).ˆalpha)./L(:,n)...
//.*delta+drift(:,f).*delta);
//end
//L(:,n)=L(:,n).*exp(increment(:,1));
//L(L(:,n)<.00001,n)=.00001;
//end
//B(:,t)=1.0;
//for n=t+1:T_e,
//B(:,n)=B(:,n-1)./(1+delta.*L(:,n-1));
//end
//money_market(t+1,:)=money_market(t,:).*(1+delta*L(:,n_spot))';
//if((t>= T_s) & (t <=T_x))
//for n=t:(T_e-1), %//the swap leg is determined one date before
//%//the end
//swap(t,:)=swap(t,:)+ (B(:,n+1).*
//(L(:,n)-k).*pr_flag*delta)' ;
//end
//end
//n_spot=n_spot+1;
//end
//value=repmat(0,[P,1]);
//stop_rule=repmat(T_x,[P,1]);
//value(swap(T_x,:)>0,1) = (swap(T_x,swap(T_x,:)>0))';
//coeff=repmat(0,[T_x,6]);
//for t=(T_x-1):-1:T_s,
//i=0;
//a=0;
//y=0;
//for p=1:P,
//if (swap(t,p)> 0.0)
//i=i+1;
//a(i,1)=1;
//a(i,2)=swap(t,p);
//a(i,3)=swap(t,p)*swap(t,p);
//a(i,4)=money_market(t,p);
//a(i,5)=money_market(t,p)*money_market(t,p);
//a(i,6)=money_market(t,p)*swap(t,p);
//y(i,1)= money_market(t,p)/money_market(stop_rule(p,1),p) *
//value(p,1);“London” — 2006/8/23 — 22:09 — page 65 — #67
//Section 1.13 Bermudan Swaption Pricing in Matlab 65
//end
//end
//temp=inv(a'*a)*(a'*y);
//coeff(t,:)=temp';
//expec_cont_value=repmat(0,[P,1]);
//exer_value=repmat(0,[P,1]);
//expec_cont_value(:,1)=(coeff(t,1)+coeff(t,2).*swap(t,:)+
//coeff(t,3).*swap(t,:)...
//.*swap(t,:)+coeff(t,4).*money_market(t,:)+
//coeff(t,5).*money_market(t,:)...
//.*money_market(t,:)+coeff(t,6).*money_market(t,:).*swap(t,:))';
//exer_value(swap(t,:)>0,1)=(swap(t,swap(t,:)>0))';
//value((exer_value(:,1)>expec_cont_value(:,1))&(swap(t,:)>0)',1)...
//=exer_value((exer_value(:,1)> expec_cont_value(:,1))&(swap(t,:)>0)',1);
//stop_rule((exer_value(:,1)>expec_cont_value(:,1))&(swap(t,:)>0)',1)=t;
//end
//price=0;
//for p=1:P,
//price=price+ (value(p,1)/(money_market(stop_rule(p,1),p)))/P;
//end
//price
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	