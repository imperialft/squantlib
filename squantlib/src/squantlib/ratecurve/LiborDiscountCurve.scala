//package squantlib.ratecurve
//import scala.collection.immutable.TreeMap
//import org.jquantlib.time.{ Date => JDate }
//import org.jquantlib.time.{ RelativeDate => JRDate }
//
//class LiborDiscountCurve (val cashrate : CashCurve, val swaprate:SwapCurve, val basisswap:BasisSwapCurve, val tenorbasisswap:TenorBasisSwapCurve, vdate : JDate) 
//extends RateCurve{
//	val valuedate = vdate.toAbsolute(null)
//	
//	def getZC(spread : Double) : ZCCurve = {
//	  var ZC : TreeMap[JDate, Double] = TreeMap.empty
//	  
//	  
//            Vector[col_ZC].Add(new DateParameter("0M", market.MarketDate), 1.00, false, true);
//            double dcfFloat1 = DateServices.GetSimpleDayFrac(FloatBasis);
//            double dcfFix1 = DateServices.GetSimpleDayFrac(FixBasis);
//            double dcfFloat2 = (isRefin ? dcfFloat1 : DateServices.GetSimpleDayFrac(refin.FloatBasis));
//            double dcfFix2 = (isRefin ? dcfFix1 : DateServices.GetSimpleDayFrac(refin.FixBasis));
//
//            double bsccy, zcspd;
//            int fixStep = DateServices.StringToMonths(FixPeriod);
//            int i = 0;
//            IDateParameter maturity;
//
//            do
//            {
//                i += ZCPeriod_months;
//                maturity = new DateParameter(DateServices.MonthsToString(i), market.MarketDate);
//                double bs3m6m = (maturity.Name == DiscountPeriod ? 0.0 : Basis3m6mCurve.Get(maturity).Value.Value);
//
//                if (isRefin)
//                {
//                    zcspd = Vector[col_ZCSpread].Get(maturity).Value.Value;
//                }
//                else
//                {
//                    if (Name == Market.PivotCurve.Name)
//                    {
//                        bsccy = -refin.BasisSwapCurve.Get(maturity).Value.Value;
//                        zcspd = (bsccy + refin.Rate(col_ZCSpread, maturity).Value) * dcfFloat2 / dcfFloat1;
//                    }
//                    else
//                    {
//                        bsccy = BasisSwap(maturity).Value;
//                        zcspd = bsccy + refin.Rate(col_ZCSpread, maturity).Value * dcfFloat1 / dcfFloat2;
//                    }
//                }
//
//                Vector[col_ZCSpread].Get(maturity).Value = zcspd;
//                Vector[col_ZC].Get(maturity).Value = 1 / (1 + (Swap(maturity) + Rate(col_ZCSpread, maturity) - bs3m6m) * dcfFloat1 *
//                            DateServices.GetDayFrac(market.MarketDate, maturity.Date, CurveKeys.base_30_360));
//
//            } while ((string)Vector[CurveKeys.RateType].Get(maturity).Content != CurveKeys.Swap);
//
//            IDateParameter prevmaturity = new DateParameter(DateServices.MonthsToString(i - fixStep), market.MarketDate);
//
//            int maxmonths = DateServices.StringToMonths(MaxMaturity.Name);
//            while (i <= maxmonths)
//            {
//                maturity = new DateParameter(DateServices.MonthsToString(i), market.MarketDate);
//                zcspd = 0.00;
//
//                double floatDuration1, floatDuration2;
//                int maturity_months = DateServices.StringToMonths(prevmaturity.Name);
//                int fixperiod_months = DateServices.StringToMonths(FixPeriod);
//                double bs3m6m = (FloatPeriod == CcyBSPeriod ? 0.0 : Basis3m6m(maturity).Value);
//
//                if (isRefin) 
//                {
//                    zcspd = Rate(col_ZCSpread, maturity).Value;
//                }
//                else
//                {
//                    if (i <= fixStep)
//                    {
//                        floatDuration1 = dcfFloat1;
//                        floatDuration2 = dcfFloat2;
//                    }
//                    else
//                    {
//                        floatDuration1 = QuickDuration(maturity_months, fixperiod_months, FloatBasis, col_ZC, true);
//                        floatDuration2 = refin.QuickDuration(maturity_months, fixperiod_months, refin.FloatBasis, col_ZC, true);
//                    }
//
//                    if (Name == Market.PivotCurve.Name) // this currency = Pivot currency <> refin currency
//                    {
//                        bsccy = -refin.BasisSwap(maturity).Value;
//                        zcspd = (bsccy + refin.Rate(col_ZCSpread, maturity).Value) * floatDuration2 / floatDuration1;
//                        bs3m6m = bs3m6m * floatDuration2 / floatDuration1;
//                    }
//                    else // refin currency = Pivot Currency
//                    {
//                        bsccy = BasisSwap(maturity).Value;
//                        zcspd = bsccy + refin.Rate(col_ZCSpread, maturity).Value * floatDuration2 / floatDuration1;
//                    }
//                }
//
//                double realRate = Swap(maturity).Value + (zcspd - bs3m6m) * dcfFloat1 / dcfFix1;
//                double fixDuration1 = QuickDuration(maturity_months, fixperiod_months, FixBasis, col_ZC);
//                double dcf = DateServices.GetDayFrac(prevmaturity.Date, maturity.Date, FixBasis);
//                Vector[col_ZCSpread].Get(maturity).Value = zcspd;
//                Vector[col_ZC].Get(maturity).Value = (1 - realRate * fixDuration1) / (1 + realRate * dcf);
//                i += fixStep;
//                prevmaturity.Name = maturity.Name;
//            }
//
//            Vector[col_ZCSpread].ApplyInterpolation();
//        }
//
//  
//}