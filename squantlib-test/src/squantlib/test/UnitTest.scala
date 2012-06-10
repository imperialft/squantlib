package squantlib.test

import org.junit._
import org.junit.Assert._

class UnitTest {
  @Before def setUp():Unit = {}
  @After def tearDown():Unit = {}
  @Test def testTruth():Unit = assertTrue(true)
}

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.collection.Iterable
import squantlib.parameter._
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit 

class PeriodTest {
  val origin = new JDate(1, 10, 2000)
  private def periodify(date:JDate):JPeriod = date.ToPeriod(origin)
  @Test def testConversion():Unit = {
    assertEquals(periodify(new JDate(10, 10, 2000)).toString, "9 days")
    assertEquals(periodify(new JDate(1, 1, 2002)  ).toString, "15 months")
    assertEquals(periodify(new JDate(5, 2, 2005)  ).toString, "1588 days")
    assertEquals(periodify(new JDate(1, 10, 2012) ).toString, "12 years")
  }
}

class VectorTest {
  val origin = new JDate(1, 10, 2000)
  @Test def testVector1():Unit = {
    // excepted data
    val expected = List(
      Tuple("0 months", 20.0, 20.0, 20.0, 20.0),
      Tuple("3 months", 20.0, 19.444196428571427, 19.37682064845543, 19.378480225374958),
      Tuple("6 months", 20.0, 18.841517857142858, 18.717778251965356, 18.72082616313508),
      Tuple("9 months", 20.0, 18.232142857142858, 18.0872856617998, 18.09085373414873),
      Tuple("12 months", 20.0, 17.61607142857143, 17.505782427571603, 17.50849902810999),
      Tuple("15 months", 20.0, 17.0, 17.0, 17.0),
      Tuple("18 months", 20.0, 16.840848806366047, 16.592448835328373, 16.587711703440625),
      Tuple("21 months", 20.0, 16.679929266136163, 16.262199971010883, 16.251081028345748),
      Tuple("24 months", 20.0, 16.517241379310345, 16.000756703029836, 15.982270761934295),
      Tuple("27 months", 20.0, 16.354553492484527, 15.800790814261623, 15.774739221480276),
      Tuple("30 months", 20.0, 16.195402298850574, 15.653713551511023, 15.62076406830737),
      Tuple("33 months", 20.0, 16.03448275862069, 15.54284700664492, 15.504125900863109),
      Tuple("36 months", 20.0, 15.871794871794872, 15.458236384918472, 15.41562206608664),
      Tuple("39 months", 20.0, 15.709106984969054, 15.389896988871804, 15.346063306981561),
      Tuple("42 months", 20.0, 15.548187444739169, 15.32713859885406, 15.285438905921579),
      Tuple("45 months", 20.0, 15.387267904509283, 15.258124671262602, 15.222584054324681),
      Tuple("48 months", 20.0, 15.224580017683467, 15.170742938401771, 15.146235632481453),
      Tuple("51 months", 20.0, 15.061892130857649, 15.05430242521223, 15.046382653420046),
      Tuple("54 months", 20.0, 14.803220035778175, 14.901851486112557, 14.916174650456435),
      Tuple("57 months", 20.0, 14.477638640429339, 14.70617413569687, 14.748887495111802),
      Tuple("60 months", 20.0, 14.148479427549194, 14.468965981177057, 14.545649100411076),
      Tuple("63 months", 20.0, 13.819320214669052, 14.195414738361276, 14.310576366788558),
      Tuple("66 months", 20.0, 13.49731663685152, 13.895779556128812, 14.052186643427909),
      Tuple("69 months", 20.0, 13.171735241502683, 13.563748223013068, 13.764721726984241),
      Tuple("72 months", 20.0, 12.84257602862254, 13.20156860158618, 13.449746733063062),
      Tuple("75 months", 20.0, 12.513416815742398, 12.815997190563527, 13.112752539986925),
      Tuple("78 months", 20.0, 12.191413237924866, 12.419309095240534, 12.764154441202649),
      Tuple("81 months", 20.0, 11.865831842576029, 12.001748139813113, 12.395028997515746),
      Tuple("84 months", 20.0, 11.536672629695886, 11.565979921322347, 12.007257904536017),
      Tuple("87 months", 20.0, 11.207513416815743, 11.119771211936019, 11.607282752168166),
      Tuple("90 months", 20.0, 10.881932021466906, 10.671341484691318, 11.20207429971617),
      Tuple("93 months", 20.0, 10.556350626118068, 10.219049997589046, 10.789740269729158),
      Tuple("96 months", 20.0, 10.227191413237925, 9.761085794604156, 10.36807431208384),
      Tuple("99 months", 20.0, 9.898032200357783, 9.305667882434044, 9.944046325611119),
      Tuple("102 months", 20.0, 9.57602862254025, 8.8657500906391, 9.52932729294912),
      Tuple("105 months", 20.0, 9.250447227191414, 8.42971694682489, 9.112472730098565),
      Tuple("108 months", 20.0, 8.92128801431127, 8.001070661448388, 8.695998360020322),
      Tuple("111 months", 20.0, 8.592128801431127, 7.587921965586865, 8.28697710088151),
      Tuple("114 months", 20.0, 8.270125223613595, 7.201886528162169, 7.896435962479338),
      Tuple("117 months", 20.0, 7.9445438282647585, 6.832936088024406, 7.51357810062104),
      Tuple("120 months", 20.0, 7.615384615384615, 6.4849905900994855, 7.141235818271777),
      Tuple("123 months", 20.0, 7.286225402504472, 6.165493980389751, 6.786161786628782),
      Tuple("126 months", 20.0, 6.964221824686941, 5.883606748268864, 6.457892510036126),
      Tuple("129 months", 20.0, 6.638640429338103, 5.632563372195923, 6.147548583025884),
      Tuple("132 months", 20.0, 6.309481216457961, 5.416701531565785, 5.858278888567504),
      Tuple("135 months", 20.0, 5.980322003577818, 5.242239877851032, 5.59609258458223),
      Tuple("138 months", 20.0, 5.65474060822898, 5.113601945723927, 5.365824490108551),
      Tuple("141 months", 20.0, 5.329159212880143, 5.031822085137222, 5.166868834676039),
      Tuple("144 months", 20.0, 5.0, 5.0, 5.0),
      Tuple("147 months", 20.0, 5.0, 5.0, 4.856603529125241),
      Tuple("150 months", 20.0, 5.0, 5.0, 4.720304583726382),
      Tuple("153 months", 20.0, 5.0, 5.0, 4.586379971784689),
      Tuple("156 months", 20.0, 5.0, 5.0, 4.45484583137577),
      Tuple("159 months", 20.0, 5.0, 5.0, 4.327083997273686),
      Tuple("162 months", 20.0, 5.0, 5.0, 4.205645839527482),
      Tuple("165 months", 20.0, 5.0, 5.0, 4.086323139682068),
      Tuple("168 months", 20.0, 5.0, 5.0, 3.969130276265214),
      Tuple("171 months", 20.0, 5.0, 5.0, 3.8552984214534964),
      Tuple("174 months", 20.0, 5.0, 5.0, 3.7471007672923706),
      Tuple("177 months", 20.0, 5.0, 5.0, 3.640787920893402),
      Tuple("180 months", 20.0, 5.0, 5.0, 3.536372693081489),
      Tuple("183 months", 20.0, 5.0, 5.0, 3.434952020304338),
      Tuple("186 months", 20.0, 5.0, 5.0, 3.337495466771),
      Tuple("189 months", 20.0, 5.0, 5.0, 3.242803953264554),
      Tuple("192 months", 20.0, 5.0, 5.0, 3.149802624737182),
      Tuple("195 months", 20.0, 5.0, 5.0, 3.0594685086693096),
      Tuple("198 months", 20.0, 5.0, 5.0, 2.9736055534760633),
      Tuple("201 months", 20.0, 5.0, 5.0, 2.8892383346338923),
      Tuple("204 months", 20.0, 5.0, 5.0, 2.8063770184933787),
      Tuple("207 months", 20.0, 5.0, 5.0, 2.725892106414183),
      Tuple("210 months", 20.0, 5.0, 5.0, 2.649390860811735),
      Tuple("213 months", 20.0, 5.0, 5.0, 2.5742222701789736),
      Tuple("216 months", 20.0, 5.0, 5.0, 2.500395392420798),
      Tuple("219 months", 20.0, 5.0, 5.0, 2.428685817407868),
      Tuple("222 months", 20.0, 5.0, 5.0, 2.360525566394444),
      Tuple("225 months", 20.0, 5.0, 5.0, 2.2935526698682933),
      Tuple("228 months", 20.0, 5.0, 5.0, 2.2277751981433953),
      Tuple("231 months", 20.0, 5.0, 5.0, 2.163884177880179),
      Tuple("234 months", 20.0, 5.0, 5.0, 2.1024903962567487),
      Tuple("237 months", 20.0, 5.0, 5.0, 2.042838480700161),
      Tuple("240 months", 20.0, 5.0, 5.0, 1.9842513149602494),
      Tuple("243 months", 20.0, 5.0, 5.0, 1.927344387781469),
      Tuple("246 months", 20.0, 5.0, 5.0, 1.8732541154543936),
      Tuple("249 months", 20.0, 5.0, 5.0, 1.820106097984224),
      Tuple("252 months", 20.0, 5.0, 5.0, 1.7679067397705108),
      Tuple("255 months", 20.0, 5.0, 5.0, 1.7172044223067524),
      Tuple("258 months", 20.0, 5.0, 5.0, 1.6690116574679015),
      Tuple("261 months", 20.0, 5.0, 5.0, 1.6216584126533282),
      Tuple("264 months", 20.0, 5.0, 5.0, 1.5751503939855578),
      Tuple("267 months", 20.0, 5.0, 5.0, 1.5299761924666546),
      Tuple("270 months", 20.0, 5.0, 5.0, 1.4870379249576893),
      Tuple("273 months", 20.0, 5.0, 5.0, 1.444847643904825),
      Tuple("276 months", 20.0, 5.0, 5.0, 1.4034104332872923),
      Tuple("279 months", 20.0, 5.0, 5.0, 1.3631616126228494),
      Tuple("282 months", 20.0, 5.0, 5.0, 1.3244859537228453),
      Tuple("285 months", 20.0, 5.0, 5.0, 1.2869076016846968),
      Tuple("288 months", 20.0, 5.0, 5.0, 1.25),
      Tuple("291 months", 20.0, 5.0, 5.0, 1.2141508822813103),
      Tuple("294 months", 20.0, 5.0, 5.0, 1.1800761459315954),
      Tuple("297 months", 20.0, 5.0, 5.0, 1.1465949929461723),
      Tuple("300 months", 20.0, 5.0, 5.0, 1.1137114578439422)
    )
    // preparing
    var testmap:TreeMap[JPeriod, Double] = TreeMap.empty
    val d1 = (new JDate(10, 10, 2000).ToPeriod(origin), 20.0)
    val d2 = (new JDate(1, 1, 2002).ToPeriod(origin), 17.0)
    val d3 = (new JDate(5, 2, 2005).ToPeriod(origin), 15.0)
    val d4 = (new JDate(1, 10, 2012).ToPeriod(origin), 5.0)
    testmap ++= Map(d2)
    testmap ++= Map(d4)
    testmap ++= Map(d1)
    testmap ++= Map(d3)
    // interpolating
    var flat = new FlatVector(origin, Map(d1))
    var linear = new LinearNoExtrapolation(origin, testmap)
    var spline = new SplineNoExtrapolation(origin, testmap, 2)
    var spline2 = new SplineEExtrapolation(origin, testmap, 1)
    var mindate = new JDate(5, 5, 2000)
    val testcase = 100
    val testperiod = 3
    var inputset = for (i <- 0 to (testcase * testperiod) if i % testperiod == 0) yield new JPeriod(i, TimeUnit.Months)
    // assertions
    assertEquals(flat.maxDate.toString(), "October 10, 2000")
    assertEquals(linear.maxDate.toString(), "October 1, 2012")
    assertEquals(spline.maxDate.toString(), "October 1, 2012")
    assertEquals(spline2.maxDate.toString(), "October 1, 2012")
    assertEquals(inputset.size, expected.size)
    var i = 0
    inputset.foreach((d:JPeriod) => {
      val tuple = Tuple(d.toString(), flat.value(d), linear.value(d), spline.value(d), spline2.value(d))
      assertEquals(tuple, expected(i))
      i += 1
    })
  }
}