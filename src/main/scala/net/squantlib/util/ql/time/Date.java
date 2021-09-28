/*
 Copyright (C) 2008 Srinivas Hasti

 This source code is release under the BSD License.

 This file is part of JQuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://jquantlib.org/

 JQuantLib is free software: you can redistribute it and/or modify it
 under the terms of the JQuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <jquant-devel@lists.sourceforge.net>. The license is also available online at
 <http://www.jquantlib.org/index.php/LICENSE.TXT>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.

 JQuantLib is based on QuantLib. http://quantlib.org/
 When applicable, the original copyright notice follows this notice.
 */

package net.squantlib.util.ql.time;

import net.squantlib.util.ql.QL;
import net.squantlib.util.ql.Settings;
import net.squantlib.util.ql.lang.exceptions.LibraryException;
import net.squantlib.util.ql.util.DefaultObservable;
import net.squantlib.util.ql.util.Observable;
import net.squantlib.util.ql.util.Observer;

import java.io.Serializable;
import java.util.Calendar;
import java.util.*;


/**
 * Date class to represent time in days.
 *
 * @author Richard Gomes
 * @author Masakatsu Wakayu
 */
public class Date implements Observable, Comparable<Date>, Serializable, Cloneable {

  private static final long serialVersionUID = -7150540867519744332L;

  // Absolute date definition
  protected /* @NonNegative */ long serialNumber;

  private static final int monthLength[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  private static final int monthLeapLength[] = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  private static final int monthOffset[] = {
    0, 31, 59, 90, 120, 151,        // Jan - Jun
    181, 212, 243, 273, 304, 334,   // Jun - Dec
    365 // used in dayOfMonth to bracket day
  };

  private static final int monthLeapOffset[] = {
    0, 31, 60, 91, 121, 152,        // Jan - Jun
    182, 213, 244, 274, 305, 335,   // Jun - Dec
    366 // used in dayOfMonth to bracket day
  };

  // the list of all December 31st in the preceding year
  // e.g. for 1901 yearOffset[1] is 366, that is, December 31 1900
  private static final int maxCalculationYear = 2999;

  private static final int yearOffset[] = {
//
//    static {
//        yearOffset = new int[maxCalculationYear - 1900 + 1];
//        int currentD = 1;
//        try {
//
//            for (int y = 0; y <= maxCalculationYear - 1900; y++) {
//                if (y == 0) {
//                    yearOffset[y] = 0;
//                } else if (isLeap(y)) {
//                    currentD += 366;
//                    yearOffset[y] = currentD;
//                } else {
//                    currentD += 365;
//                    yearOffset[y] = currentD;
//                }
//            }
//
//            for (int y = 0; y <= maxCalculationYear - 1900; y++) {
//                System.out.println(y);
//                System.out.println(currentD);
//                System.out.println(yearOffset.toString());
//            }
//        } catch (ExceptionInInitializerError e) {
//            e.getCause().printStackTrace();
//        }
//
//
//
//    }

//        // 1900-1909
//            0,  366,  731, 1096, 1461, 1827, 2192, 2557, 2922, 3288,
//        // 1910-1919
//         3653, 4018, 4383, 4749, 5114, 5479, 5844, 6210, 6575, 6940,
//        // 1920-1929
//         7305, 7671, 8036, 8401, 8766, 9132, 9497, 9862,10227,10593,
//        // 1930-1939
//        10958,11323,11688,12054,12419,12784,13149,13515,13880,14245,
//        // 1940-1949
//        14610,14976,15341,15706,16071,16437,16802,17167,17532,17898,
//        // 1950-1959
//        18263,18628,18993,19359,19724,20089,20454,20820,21185,21550,
//        // 1960-1969
//        21915,22281,22646,23011,23376,23742,24107,24472,24837,25203,
//        // 1970-1979
//        25568,25933,26298,26664,27029,27394,27759,28125,28490,28855,
//        // 1980-1989
//        29220,29586,29951,30316,30681,31047,31412,31777,32142,32508,
//        // 1990-1999
//        32873,33238,33603,33969,34334,34699,35064,35430,35795,36160,
//        // 2000-2009
//        36525,36891,37256,37621,37986,38352,38717,39082,39447,39813,
//        // 2010-2019
//        40178,40543,40908,41274,41639,42004,42369,42735,43100,43465,
//        // 2020-2029
//        43830,44196,44561,44926,45291,45657,46022,46387,46752,47118,
//        // 2030-2039
//        47483,47848,48213,48579,48944,49309,49674,50040,50405,50770,
//        // 2040-2049
//        51135,51501,51866,52231,52596,52962,53327,53692,54057,54423,
//        // 2050-2059
//        54788,55153,55518,55884,56249,56614,56979,57345,57710,58075,
//        // 2060-2069
//        58440,58806,59171,59536,59901,60267,60632,60997,61362,61728,
//        // 2070-2079
//        62093,62458,62823,63189,63554,63919,64284,64650,65015,65380,
//        // 2080-2089
//        65745,66111,66476,66841,67206,67572,67937,68302,68667,69033,
//        // 2090-2099
//        69398,69763,70128,70494,70859,71224,71589,71955,72320,72685,
//        // 2100-2109
//        73050,73415,73780,74145,74510,74876,75241,75606,75971,76337,
//        // 2110-2119
//        76702,77067,77432,77798,78163,78528,78893,79259,79624,79989,
//        // 2120-2129
//        80354,80720,81085,81450,81815,82181,82546,82911,83276,83642,
//        // 2130-2139
//        84007,84372,84737,85103,85468,85833,86198,86564,86929,87294,
//        // 2140-2149
//        87659,88025,88390,88755,89120,89486,89851,90216,90581,90947,
//        // 2150-2159
//        91312,91677,92042,92408,92773,93138,93503,93869,94234,94599,
//        // 2160-2169
//        94964,95330,95695,96060,96425,96791,97156,97521,97886,98252,
//        // 2170-2179
//        98617,98982,99347,99713,100078,100443,100808,101174,101539,101904,
//        // 2180-2189
//        102269,102635,103000,103365,103730,104096,104461,104826,105191,105557,
//        // 2190-2199
//        105922,106287,106652,107018,107383,107748,108113,108479,108844,109209,
//        // 2200
//        109574
//
    0, 366, 731, 1096, 1461, 1827, 2192, 2557, 2922, 3288,
    3653, 4018, 4383, 4749, 5114, 5479, 5844, 6210, 6575, 6940,
    7305, 7671, 8036, 8401, 8766, 9132, 9497, 9862, 10227, 10593,
    10958, 11323, 11688, 12054, 12419, 12784, 13149, 13515, 13880, 14245,
    14610, 14976, 15341, 15706, 16071, 16437, 16802, 17167, 17532, 17898,
    18263, 18628, 18993, 19359, 19724, 20089, 20454, 20820, 21185, 21550,
    21915, 22281, 22646, 23011, 23376, 23742, 24107, 24472, 24837, 25203,
    25568, 25933, 26298, 26664, 27029, 27394, 27759, 28125, 28490, 28855,
    29220, 29586, 29951, 30316, 30681, 31047, 31412, 31777, 32142, 32508,
    32873, 33238, 33603, 33969, 34334, 34699, 35064, 35430, 35795, 36160,
    36525, 36891, 37256, 37621, 37986, 38352, 38717, 39082, 39447, 39813,
    40178, 40543, 40908, 41274, 41639, 42004, 42369, 42735, 43100, 43465,
    43830, 44196, 44561, 44926, 45291, 45657, 46022, 46387, 46752, 47118,
    47483, 47848, 48213, 48579, 48944, 49309, 49674, 50040, 50405, 50770,
    51135, 51501, 51866, 52231, 52596, 52962, 53327, 53692, 54057, 54423,
    54788, 55153, 55518, 55884, 56249, 56614, 56979, 57345, 57710, 58075,
    58440, 58806, 59171, 59536, 59901, 60267, 60632, 60997, 61362, 61728,
    62093, 62458, 62823, 63189, 63554, 63919, 64284, 64650, 65015, 65380,
    65745, 66111, 66476, 66841, 67206, 67572, 67937, 68302, 68667, 69033,
    69398, 69763, 70128, 70494, 70859, 71224, 71589, 71955, 72320, 72685,
    73050, 73415, 73780, 74145, 74510, 74876, 75241, 75606, 75971, 76337,
    76702, 77067, 77432, 77798, 78163, 78528, 78893, 79259, 79624, 79989,
    80354, 80720, 81085, 81450, 81815, 82181, 82546, 82911, 83276, 83642,
    84007, 84372, 84737, 85103, 85468, 85833, 86198, 86564, 86929, 87294,
    87659, 88025, 88390, 88755, 89120, 89486, 89851, 90216, 90581, 90947,
    91312, 91677, 92042, 92408, 92773, 93138, 93503, 93869, 94234, 94599,
    94964, 95330, 95695, 96060, 96425, 96791, 97156, 97521, 97886, 98252,
    98617, 98982, 99347, 99713, 100078, 100443, 100808, 101174, 101539, 101904,
    102269, 102635, 103000, 103365, 103730, 104096, 104461, 104826, 105191, 105557,
    105922, 106287, 106652, 107018, 107383, 107748, 108113, 108479, 108844, 109209,
    109574, 109939, 110304, 110669, 111034, 111400, 111765, 112130, 112495, 112861,
    113226, 113591, 113956, 114322, 114687, 115052, 115417, 115783, 116148, 116513,
    116878, 117244, 117609, 117974, 118339, 118705, 119070, 119435, 119800, 120166,
    120531, 120896, 121261, 121627, 121992, 122357, 122722, 123088, 123453, 123818,
    124183, 124549, 124914, 125279, 125644, 126010, 126375, 126740, 127105, 127471,
    127836, 128201, 128566, 128932, 129297, 129662, 130027, 130393, 130758, 131123,
    131488, 131854, 132219, 132584, 132949, 133315, 133680, 134045, 134410, 134776,
    135141, 135506, 135871, 136237, 136602, 136967, 137332, 137698, 138063, 138428,
    138793, 139159, 139524, 139889, 140254, 140620, 140985, 141350, 141715, 142081,
    142446, 142811, 143176, 143542, 143907, 144272, 144637, 145003, 145368, 145733,
    146098, 146463, 146828, 147193, 147558, 147924, 148289, 148654, 149019, 149385,
    149750, 150115, 150480, 150846, 151211, 151576, 151941, 152307, 152672, 153037,
    153402, 153768, 154133, 154498, 154863, 155229, 155594, 155959, 156324, 156690,
    157055, 157420, 157785, 158151, 158516, 158881, 159246, 159612, 159977, 160342,
    160707, 161073, 161438, 161803, 162168, 162534, 162899, 163264, 163629, 163995,
    164360, 164725, 165090, 165456, 165821, 166186, 166551, 166917, 167282, 167647,
    168012, 168378, 168743, 169108, 169473, 169839, 170204, 170569, 170934, 171300,
    171665, 172030, 172395, 172761, 173126, 173491, 173856, 174222, 174587, 174952,
    175317, 175683, 176048, 176413, 176778, 177144, 177509, 177874, 178239, 178605,
    178970, 179335, 179700, 180066, 180431, 180796, 181161, 181527, 181892, 182257,
    182622, 182988, 183353, 183718, 184083, 184449, 184814, 185179, 185544, 185910,
    186275, 186640, 187005, 187371, 187736, 188101, 188466, 188832, 189197, 189562,
    189927, 190293, 190658, 191023, 191388, 191754, 192119, 192484, 192849, 193215,
    193580, 193945, 194310, 194676, 195041, 195406, 195771, 196137, 196502, 196867,
    197232, 197598, 197963, 198328, 198693, 199059, 199424, 199789, 200154, 200520,
    200885, 201250, 201615, 201981, 202346, 202711, 203076, 203442, 203807, 204172,
    204537, 204903, 205268, 205633, 205998, 206364, 206729, 207094, 207459, 207825,
    208190, 208555, 208920, 209286, 209651, 210016, 210381, 210747, 211112, 211477,
    211842, 212208, 212573, 212938, 213303, 213669, 214034, 214399, 214764, 215130,
    215495, 215860, 216225, 216591, 216956, 217321, 217686, 218052, 218417, 218782,
    219147, 219512, 219877, 220242, 220607, 220973, 221338, 221703, 222068, 222434,
    222799, 223164, 223529, 223895, 224260, 224625, 224990, 225356, 225721, 226086,
    226451, 226817, 227182, 227547, 227912, 228278, 228643, 229008, 229373, 229739,
    230104, 230469, 230834, 231200, 231565, 231930, 232295, 232661, 233026, 233391,
    233756, 234122, 234487, 234852, 235217, 235583, 235948, 236313, 236678, 237044,
    237409, 237774, 238139, 238505, 238870, 239235, 239600, 239966, 240331, 240696,
    241061, 241427, 241792, 242157, 242522, 242888, 243253, 243618, 243983, 244349,
    244714, 245079, 245444, 245810, 246175, 246540, 246905, 247271, 247636, 248001,
    248366, 248732, 249097, 249462, 249827, 250193, 250558, 250923, 251288, 251654,
    252019, 252384, 252749, 253115, 253480, 253845, 254210, 254576, 254941, 255306,
    255671, 256036, 256401, 256766, 257131, 257497, 257862, 258227, 258592, 258958,
    259323, 259688, 260053, 260419, 260784, 261149, 261514, 261880, 262245, 262610,
    262975, 263341, 263706, 264071, 264436, 264802, 265167, 265532, 265897, 266263,
    266628, 266993, 267358, 267724, 268089, 268454, 268819, 269185, 269550, 269915,
    270280, 270646, 271011, 271376, 271741, 272107, 272472, 272837, 273202, 273568,
    273933, 274298, 274663, 275029, 275394, 275759, 276124, 276490, 276855, 277220,
    277585, 277951, 278316, 278681, 279046, 279412, 279777, 280142, 280507, 280873,
    281238, 281603, 281968, 282334, 282699, 283064, 283429, 283795, 284160, 284525,
    284890, 285256, 285621, 285986, 286351, 286717, 287082, 287447, 287812, 288178,
    288543, 288908, 289273, 289639, 290004, 290369, 290734, 291100, 291465, 291830,
    292195, 292560, 292925, 293290, 293655, 294021, 294386, 294751, 295116, 295482,
    295847, 296212, 296577, 296943, 297308, 297673, 298038, 298404, 298769, 299134,
    299499, 299865, 300230, 300595, 300960, 301326, 301691, 302056, 302421, 302787,
    303152, 303517, 303882, 304248, 304613, 304978, 305343, 305709, 306074, 306439,
    306804, 307170, 307535, 307900, 308265, 308631, 308996, 309361, 309726, 310092,
    310457, 310822, 311187, 311553, 311918, 312283, 312648, 313014, 313379, 313744,
    314109, 314475, 314840, 315205, 315570, 315936, 316301, 316666, 317031, 317397,
    317762, 318127, 318492, 318858, 319223, 319588, 319953, 320319, 320684, 321049,
    321414, 321780, 322145, 322510, 322875, 323241, 323606, 323971, 324336, 324702,
    325067, 325432, 325797, 326163, 326528, 326893, 327258, 327624, 327989, 328354,
    328719, 329085, 329450, 329815, 330180, 330546, 330911, 331276, 331641, 332007,
    332372, 332737, 333102, 333468, 333833, 334198, 334563, 334929, 335294, 335659,
    336024, 336390, 336755, 337120, 337485, 337851, 338216, 338581, 338946, 339312,
    339677, 340042, 340407, 340773, 341138, 341503, 341868, 342234, 342599, 342964,
    343329, 343695, 344060, 344425, 344790, 345156, 345521, 345886, 346251, 346617,
    346982, 347347, 347712, 348078, 348443, 348808, 349173, 349539, 349904, 350269,
    350634, 351000, 351365, 351730, 352095, 352461, 352826, 353191, 353556, 353922,
    354287, 354652, 355017, 355383, 355748, 356113, 356478, 356844, 357209, 357574,
    357939, 358305, 358670, 359035, 359400, 359766, 360131, 360496, 360861, 361227,
    361592, 361957, 362322, 362688, 363053, 363418, 363783, 364149, 364514, 364879,
    365244, 365609, 365974, 366339, 366704, 367070, 367435, 367800, 368165, 368531,
    368896, 369261, 369626, 369992, 370357, 370722, 371087, 371453, 371818, 372183,
    372548, 372914, 373279, 373644, 374009, 374375, 374740, 375105, 375470, 375836,
    376201, 376566, 376931, 377297, 377662, 378027, 378392, 378758, 379123, 379488,
    379853, 380219, 380584, 380949, 381314, 381680, 382045, 382410, 382775, 383141,
    383506, 383871, 384236, 384602, 384967, 385332, 385697, 386063, 386428, 386793,
    387158, 387524, 387889, 388254, 388619, 388985, 389350, 389715, 390080, 390446,
    390811, 391176, 391541, 391907, 392272, 392637, 393002, 393368, 393733, 394098,
    394463, 394829, 395194, 395559, 395924, 396290, 396655, 397020, 397385, 397751,
    398116, 398481, 398846, 399212, 399577, 399942, 400307, 400673, 401038, 401403,
    401768


  };

//    private static final boolean yearIsLeap[] = {
//        // 1900 is leap in agreement with Excel's bug
//        // 1900 is out of valid date range anyway
//        // 1900-1909
//         true,false,false,false, true,false,false,false, true,false,
//        // 1910-1919
//        false,false, true,false,false,false, true,false,false,false,
//        // 1920-1929
//         true,false,false,false, true,false,false,false, true,false,
//        // 1930-1939
//        false,false, true,false,false,false, true,false,false,false,
//        // 1940-1949
//         true,false,false,false, true,false,false,false, true,false,
//        // 1950-1959
//        false,false, true,false,false,false, true,false,false,false,
//        // 1960-1969
//         true,false,false,false, true,false,false,false, true,false,
//        // 1970-1979
//        false,false, true,false,false,false, true,false,false,false,
//        // 1980-1989
//         true,false,false,false, true,false,false,false, true,false,
//        // 1990-1999
//        false,false, true,false,false,false, true,false,false,false,
//        // 2000-2009
//         true,false,false,false, true,false,false,false, true,false,
//        // 2010-2019
//        false,false, true,false,false,false, true,false,false,false,
//        // 2020-2029
//         true,false,false,false, true,false,false,false, true,false,
//        // 2030-2039
//        false,false, true,false,false,false, true,false,false,false,
//        // 2040-2049
//         true,false,false,false, true,false,false,false, true,false,
//        // 2050-2059
//        false,false, true,false,false,false, true,false,false,false,
//        // 2060-2069
//         true,false,false,false, true,false,false,false, true,false,
//        // 2070-2079
//        false,false, true,false,false,false, true,false,false,false,
//        // 2080-2089
//         true,false,false,false, true,false,false,false, true,false,
//        // 2090-2099
//        false,false, true,false,false,false, true,false,false,false,
//        // 2100-2109
//        false,false,false,false, true,false,false,false, true,false,
//        // 2110-2119
//        false,false, true,false,false,false, true,false,false,false,
//        // 2120-2129
//         true,false,false,false, true,false,false,false, true,false,
//        // 2130-2139
//        false,false, true,false,false,false, true,false,false,false,
//        // 2140-2149
//         true,false,false,false, true,false,false,false, true,false,
//        // 2150-2159
//        false,false, true,false,false,false, true,false,false,false,
//        // 2160-2169
//         true,false,false,false, true,false,false,false, true,false,
//        // 2170-2179
//        false,false, true,false,false,false, true,false,false,false,
//        // 2180-2189
//         true,false,false,false, true,false,false,false, true,false,
//        // 2190-2199
//        false,false, true,false,false,false, true,false,false,false,
//        // 2200
//        false
//    };


  //
  // public constructors
  //

  /**
   * Default constructor returning a null date.
   */
  public Date() {
    this(0);
  }

  /**
   * Constructor taking a serial number as given by Applix or Excel.
   *
   * @param serialNumber
   * @return
   */
  public Date(final long serialNumber) {
    this.serialNumber = serialNumber;
  }

  /**
   * More traditional constructor.
   *
   * @param d
   * @param moreGreeks
   * @param y
   */
  public Date(
    final int day,
    final Month month,
    final int year
  ) {
    this(fromDMY(day, month.value(), year));
  }

  /**
   * More traditional constructor.
   *
   * @param d
   * @param moreGreeks
   * @param y
   */
  public Date(
    final int day,
    final int month,
    final int year
  ) {
    this(fromDMY(day, month, year));
  }


  /**
   * Allows interoperability with JDK Date
   * <p>
   * <b>NOTE</b>: Both java.util.Date and JQuantLib Date do not support time zones or high precision clocks.
   * In other words, a day <i>always</i> has exactly 84,600 seconds, or 84,600,000 milliseconds.
   *
   * @author Richard Gomes
   */
  public Date(final java.util.Date date) {
//    	this(25569+(date.getTime()/86400000L));
    final Calendar c = Calendar.getInstance();
    c.setTime(date);
    final int d = c.get(Calendar.DAY_OF_MONTH);
    final int m = c.get(Calendar.MONTH);
    final int y = c.get(Calendar.YEAR);
    this.serialNumber = fromDMY(d, m + 1, y);
  }

  //
  // public methods :: inspectors
  //

  public Weekday weekday() /* @ReadOnly */ {
    final int w = (int) (serialNumber() % 7);
    return Weekday.valueOf(w == 0L ? 7 : w);
  }

  public int dayOfMonth() /* @ReadOnly */ {
    return dayOfYear() - monthOffset(month().value(), isLeap(year()));
  }

  /**
   * One-based (Jan 1st = 1)
   *
   * @return
   */
  public int dayOfYear() /* @ReadOnly */ {
    return (int) (serialNumber() - yearOffset(year()));
  }

  public Month month() /* @ReadOnly */ {
    final int d = dayOfYear(); // dayOfYear is 1 based
    int m = d / 30 + 1;
    final boolean leap = isLeap(year());
    while (d <= monthOffset(m, leap)) {
      --m;
    }
    while (d > monthOffset(m + 1, leap)) {
      ++m;
    }
    return Month.valueOf(m);
  }

  public int year() /* @ReadOnly */ {
    int y = (int) (serialNumber() / 365) + 1900;
    if (serialNumber() <= yearOffset(y)) {
      --y;
    }
    return y;
  }

  public long serialNumber() /* @ReadOnly */ {
    return this.serialNumber;
  }


  //
  // public methods :: name date algebra
  //

  /**
   * increments <code>this</code> date by the given number of days
   *
   * @return this
   */
  //-- Date& operator+=(BigInteger days);
  public Date addAssign(final int days) {
    serialNumber += days;
    checkSerialNumber();
    delegatedObservable.notifyObservers();
    return this;
  }

  /**
   * increments <code>this</code> date by the given period
   *
   * @return this
   */
  //-- Date& operator+=(const Period&);
  public Date addAssign(final Period period) {
    serialNumber = advance(this, period.length(), period.units());
    checkSerialNumber();
    delegatedObservable.notifyObservers();
    return this;
  }

  /**
   * decrement <code>this</code> date by the given number of days
   *
   * @return this
   */
  //-- Date& operator-=(BigInteger days);
  public Date subAssign(final int days) {
    serialNumber -= days;
    checkSerialNumber();
    delegatedObservable.notifyObservers();
    return this;
  }

  /**
   * decrements <code>this</code> date by the given period
   *
   * @return this
   */
  //-- Date& operator-=(const Period&);
  public Date subAssign(final Period period) {
    serialNumber = advance(this, -1 * period.length(), period.units());
    checkSerialNumber();
    delegatedObservable.notifyObservers();
    return this;
  }


  //    /**
  //     *  1-day pre-increment
  //     *
  //     *  @return this
  //     */
  //    //-- Date& operator++();
  //    Date inc();

  /**
   * 1-day post-increment
   *
   * @return this
   */
  //-- Date operator++(int );
  public Date inc() {
    serialNumber++;
    checkSerialNumber();
    return this;
  }

  //    /**
  //     *  1-day pre-decrement
  //     *
  //     *  @return this
  //     */
  //    //-- Date& operator--();
  //    Date dec();

  /**
   * 1-day post-decrement
   *
   * @return this
   */
  //-- Date operator--(int );
  public Date dec() {
    serialNumber--;
    checkSerialNumber();
    return this;
  }


  /**
   * returns a new date incremented by the given number of days
   *
   * @return a new instance
   */
  //-- Date operator+(BigInteger days) const;
  public Date add(final int days) /* @ReadOnly */ {
    return new Date(this.serialNumber() + days);
  }


  /**
   * returns a new date incremented by the given period
   *
   * @return a new instance
   */
  //-- Date operator+(const Period&) const;
  public Date add(final Period period) /* @ReadOnly */ {
    return new Date(advance(this, period.length(), period.units()));
  }

  /**
   * returns a new date decremented by the given number of days
   *
   * @return a new instance
   */
  //-- Date operator-(BigInteger days) const;
  public Date sub(final int days) /* @ReadOnly */ {
    return new Date(this.serialNumber() - days);
  }

  /**
   * returns a new date decremented by the given period
   *
   * @return a new instance
   */
  //-- Date operator-(const Period&) const;
  public Date sub(final Period period) /* @ReadOnly */ {
    return new Date(advance(this, -1 * period.length(), period.units()));
  }

  /**
   * Difference in days between dates
   */
  public long sub(final Date another) {
    return serialNumber() - another.serialNumber();
  }

  //
  // public methods :: relates Date
  //

  //-- bool operator==(const Date&, const Date&);
  public boolean eq(final Date another) {
    return serialNumber() == another.serialNumber();
  }

  //-- bool operator!=(const Date&, const Date&);
  public boolean ne(final Date another) {
    return serialNumber() != another.serialNumber();
  }

  //-- bool operator<(const Date&, const Date&);
  public boolean lt(final Date another) {
    return serialNumber() < another.serialNumber();
  }

  //-- bool operator<=(const Date&, const Date&);
  public boolean le(final Date another) {
    return serialNumber() <= another.serialNumber();
  }

  //-- bool operator>(const Date&, const Date&);
  public boolean gt(final Date another) {
    return serialNumber() > another.serialNumber();
  }

  //-- bool operator>=(const Date&, const Date&);
  public boolean ge(final Date another) {
    return serialNumber() >= another.serialNumber();
  }


  //
  // public methods :: not originally defined in QuantLib/C++
  //

  /**
   * @return true if this Date is <i>null or non-valid date</i>
   */
  public boolean isNull() {
    return this.serialNumber() <= 0;
  }

  public final boolean isToday() {
    final Calendar cal = Calendar.getInstance();
    final int d = cal.get(Calendar.DAY_OF_MONTH);
    final int m = cal.get(Calendar.MONTH);
    final int y = cal.get(Calendar.YEAR);
    return serialNumber() == fromDMY(d, m + 1, y);
  }


  //
  // public methods :: provide access to inner classes
  //

  /**
   * Retuirns a java.util.Date configured for a long date formatter
   * <p>
   * Method <code>toString</code> would return something like <i>Mon, dd yyyy</i>
   *
   * @return a java.util.Date configured for a long date formatter
   */
  public java.util.Date longDate() {
    return new LongDate();
  }

  /**
   * Returns a java.util.Date configured for a short date formatter
   * <p>
   * Method <code>toString</code> would return something like <i>mm/dd/yyyy</i>
   *
   * @return a java.util.Date configured for a short date formatter
   */
  public java.util.Date shortDate() {
    return new ShortDate();
  }

  /**
   * Returns a java.util.Date configured for a ISO date formatter
   * <p>
   * Method <code>toString</code> would return something like <i>yyyy-mm-yy</i>
   *
   * @return a java.util.Date configured for a ISO date formatter
   */
  public java.util.Date isoDate() {
    return new ISODate();
  }


  //
  // implements Comparable<Date>
  //

  @Override
  public int compareTo(final Date o) {
    if (this.equals(o))
      return 0;
    if (this.le(o))
      return -1;
    return 1;
  }


  //
  // implements Observable
  //

  /**
   * Implements multiple inheritance via delegate pattern to an inner class
   */
  protected final Observable delegatedObservable = new DefaultObservable(this);

  @Override
  public final void addObserver(final Observer observer) {
    delegatedObservable.addObserver(observer);
  }

  @Override
  public final int countObservers() {
    return delegatedObservable.countObservers();
  }

  @Override
  public final void deleteObserver(final Observer observer) {
    delegatedObservable.deleteObserver(observer);
  }

  @Override
  public final void notifyObservers() {
    delegatedObservable.notifyObservers();
  }

  @Override
  public final void notifyObservers(final Object arg) {
    delegatedObservable.notifyObservers(arg);
  }

  @Override
  public final void deleteObservers() {
    delegatedObservable.deleteObservers();
  }

  @Override
  public final List<Observer> getObservers() {
    return delegatedObservable.getObservers();
  }


  //
  // Overrides Object
  //

  @Override
  public int hashCode() {
    return (int) this.serialNumber();
  }

  @Override
  public boolean equals(final Object anObject) {
    if (this == anObject)
      return true;
    if (anObject == null)
      return false;

    return anObject instanceof Date &&
      ((Date) anObject).fEquals(this);
  }

  protected boolean fEquals(final Date other) {
    return eq(other);
  }


  @Override
  public String toString() {
    return shortDate().toString();
  }


  //
  // implements Cloneable
  //

  @Override
  public Date clone() {
    try {
      return (Date) super.clone();
    } catch (final CloneNotSupportedException e) {
      throw new LibraryException(e);
    }
  }


  //
  // protected methods
  //
  // These methods were not declared in QuantLib/C++
  //


  /**
   * Assigns the today's date to <code>this</code> instance
   *
   * @return this instance
   * @note Does not trigger notifications
   * @see
   */
  //TODO: consider @PackagePrivate
  protected final long todaysSerialNumber() {
    final Calendar cal = Calendar.getInstance();
    final int d = cal.get(Calendar.DAY_OF_MONTH);
    final int m = cal.get(Calendar.MONTH);
    final int y = cal.get(Calendar.YEAR);
    return fromDMY(d, m + 1, y);
  }


  /**
   * Assigns a new serialNumber to <code>this</code> instance.
   *
   * @return this instance
   * @note Does not trigger notifications
   * @see inner class DateProxy in {@link Settings}
   */
  //TODO: consider @PackagePrivate
  protected final Date assign(final long serialNumber) {
    this.serialNumber = serialNumber;
    return this;
  }


  //
  // private methods
  //

  private void checkSerialNumber() {
    QL.ensure((serialNumber() >= minimumSerialNumber()) && (serialNumber() <= maximumSerialNumber()),
      "Date's serial number is outside allowed range"); // TODO: message
  }


  private long advance(
    final Date date,
    final int n,
    final TimeUnit units
  ) {
    switch (units) {
      case Days:
        return (n + date.serialNumber());
      case Weeks:
        return (7 * n + date.serialNumber());
      case Months: {
        int d = date.dayOfMonth();
        int m = date.month().value() + n;
        int y = date.year();
        while (m > 12) {
          m -= 12;
          y += 1;
        }
        while (m < 1) {
          m += 12;
          y -= 1;
        }

        QL.ensure(y > 1900 && y <= 2999, "year out of bounds. It must be in [1901,2999]"); // TODO: message
        final int length = monthLength(m, isLeap(y));
        if (d > length) {
          d = length;
        }
        final long result = fromDMY(d, m, y);
        return result;
      }
      case Years: {
        int d = date.dayOfMonth();
        final int m = date.month().value();
        final int y = date.year() + n;

        QL.ensure(y > 1900 && y <= 2999, "year out of bounds. It must be in [1901,2999]"); // TODO: message
        if (d == 29 && m == Month.February.value() && !isLeap(y)) {
          d = 28;
        }

        final long result = fromDMY(d, m, y);
        return result;
      }
      default:
        throw new LibraryException("undefined time units"); // TODO: message
    }
  }


  //
  // public static methods
  //

  /**
   * Today's date.
   *
   * @return a new instance
   */
  public static final Date todaysDate() {
    final Calendar cal = Calendar.getInstance();
    final int d = cal.get(Calendar.DAY_OF_MONTH);
    final int m = cal.get(Calendar.MONTH);
    final int y = cal.get(Calendar.YEAR);
    return new Date(d, m + 1, y);
  }

  /**
   * Earliest allowed date
   *
   * @return a new instance
   */
  public static final Date minDate() {
    return new Date(minimumSerialNumber());
  }

  /**
   * Latest allowed date
   *
   * @return a new instance
   */
  public static final Date maxDate() {
    return new Date(maximumSerialNumber());
  }

  /**
   * Whether the given year is a leap one
   *
   * @param y
   * @return
   */
  public static final boolean isLeap(final int year) {
    //return yearIsLeap[year - 1900];
    return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0);
  }

  /**
   * Last day of the month to which the given date belongs
   *
   * @return a new instance
   */
  public static final Date endOfMonth(final Date d) {
    final int m = d.month().value();
    final int y = d.year();
    return new Date(monthLength(m, isLeap(y)), m, y);
  }

  /**
   * Whether a date is the last day of its month
   *
   * @return
   */
  public static final boolean isEndOfMonth(final Date d) {
    return (d.dayOfMonth() == monthLength(d.month().value(), isLeap(d.year())));
  }

  /**
   * Next given weekday following or equal to the given date
   * <p>
   * E.g., the Friday following Tuesday, JANUARY 15th, 2002 was JANUARY 18th, 2002.
   *
   * @return a new instance
   * @see http://www.cpearson.com/excel/DateTimeWS.htm
   */
  public static final Date nextWeekday(
    final Date d,
    final Weekday w
  ) {
    final int wd = d.weekday().value();
    final int dow = w.value();
    return new Date(d.serialNumber() + (wd > dow ? 7 : 0) - wd + dow);
  }

  /**
   * n-th given weekday in the given month and year
   * <p>
   * E.g., the 4th Thursday of March, 1998 was March 26th, 1998.
   *
   * @param n
   * @param w
   * @param m
   * @param y
   * @return a new instance
   * @see http://www.cpearson.com/excel/DateTimeWS.htm
   */
  public static final Date nthWeekday(
    final int n,
    final Weekday w,
    final Month m,
    final int y
  ) {
    return nthWeekday(n, w, m.value(), y);
  }

  /**
   * Returns a new Date which is the n-th week day of a certain month/year
   *
   * @param nth       is the desired week
   * @param dayOfWeek is the desired week day
   * @param month     is the desired month
   * @param year      is the desired year
   * @return a new instance
   */
  public static final Date nthWeekday(
    final int nth,
    final Weekday dayOfWeek,
    final int month,
    final int year
  ) {
    QL.require(nth > 0, "zeroth day of week in a given (month, year) is undefined"); // TODO: message
    QL.require(nth < 6, "no more than 5 weekday in a given (month, year)"); // TODO: message
    final int m = month;
    final int y = year;
    final int dow = dayOfWeek.value();
    // FIXME: code review
    final int first = new Date(1, m, y).weekday().value();
    final int skip = nth - (dow >= first ? 1 : 0);
    return new Date(1 + dow - first + skip * 7, m, y);
  }

  /**
   * Return the minimum Date in a range.
   */
  public static Date min(final Date... t) {
    QL.require(t != null, "argument cannot be null"); // TODO: message
    if (t.length == 0)
      return new Date();
    else {
      Date min = t[0];
      for (int i = 1; i < t.length; i++) {
        final Date curr = t[i];
        if (curr.lt(min)) {
          min = curr;
        }
      }
      return min;
    }
  }

  /**
   * Return the maximum Date in a range.
   */
  public static Date max(final Date... t) {
    QL.require(t != null, "argument cannot be null"); // TODO: message
    if (t.length == 0)
      return new Date();
    else {
      Date max = t[0];
      for (int i = 1; i < t.length; i++) {
        final Date curr = t[i];
        if (curr.gt(max)) {
          max = curr;
        }
      }
      return max;
    }
  }


  //
  // private static methods
  //

  static private long minimumSerialNumber() {
    return 367;       // Jan 1st, 1901
  }

  static private long maximumSerialNumber() {
    return 401768; //109574;    // Dec 31st, 2199
  }

  /**
   * This method is intended to calculate the integer value of a (day, month, year)
   *
   * @param d is the day as a number
   * @param m is the month as a number
   * @param y is the year as a number
   * @return
   */
  private static final long fromDMY(
    final int d,
    final int m,
    final int y
  ) {
    QL.require(y > 1900 && y <= 2999, "year(" + y + ") out of bound. It must be in [1901,2999]"); // TODO: message
    QL.require(m > 0 && m < 13, "month outside JANUARY-December range [1,12]"); // TODO: message
    final boolean leap = isLeap(y);
    final int len = monthLength(m, leap);
    final int offset = monthOffset(m, leap);
    QL.ensure(d > 0 && d <= len, "day outside month day-range"); // TODO: message
    final long result = d + offset + yearOffset(y);
    return result;
  }

  /**
   * Returns the length of a certain month
   *
   * @param m        is the desired month, as a number
   * @param leapYear if <code>true</code> means a leap year
   * @return the length of a certain month
   */
  private static final int monthLength(
    final int m,
    final boolean leapYear
  ) {
    return (leapYear ? monthLeapLength[m - 1] : monthLength[m - 1]);
  }

  /**
   * Returns the offset of a certain month
   *
   * @param m        is the desired month, as a number. If you specify 13, you will get the number of days of a year
   * @param leapYear if <code>true</code> means a leap year
   * @return the offset of a certain month or the length of an year
   * @see DefaultDate#yearOffset
   */
  private static final int monthOffset(
    final int m,
    final boolean leapYear
  ) {
    return (leapYear ? monthLeapOffset[m - 1] : monthOffset[m - 1]);
  }

  /**
   * Returns the offset of a certain year
   *
   * @param y is the desired year
   * @return the offset of a certain year
   */
  private static final long yearOffset(final int year) {
    return yearOffset[year - 1900];
  }

  /**
   * This method is equivalent to std:lower_bound function
   * Returns an index pointing to the first element in the ordered collection is equal or greater than passed value
   *
   * @param dates order collection in ascending order
   * @param value Date to be compared
   * @return index to element which is >= passed value
   */
  public static int lowerBound(
    final List<Date> dates,
    final Date value
  ) {
    int len = dates.size();
    int from = 0;
    int half;
    int middle;

    while (len > 0) {
      half = len >> 1;
      middle = from;
      middle = middle + half;

      if (value.compareTo(dates.get(middle)) == 1) { // value > 1
        from = middle;
        from++;
        len = len - half - 1;
      } else {
        len = half;
      }
    }
    return from;
  }

  /**
   * This method is equivalent to C++ sdt:upper_bound function
   * Returns an index pointing to the first element in the ordered collection which is greater than passed value
   *
   * @param dates order collection in ascending order
   * @param value Date to be compared
   * @return index to element which is > passed value
   */
  public static int upperBound(
    final List<Date> dates,
    final Date value
  ) {

    int len = dates.size();
    int from = 0;
    int half;
    int middle;

    while (len > 0) {
      half = len >> 1;
      middle = from;
      middle = middle + half;
      if (value.compareTo(dates.get(middle)) == -1) {
        len = half;
      } else {
        from = middle;
        from++;
        len = len - half - 1;
      }
    }
    return from;
  }

  /**
   * Creates new period class representing the difference from this class to target date.
   * It's always positive.
   *
   * @param target dates
   * @return new Period object
   */
  public Period ToPeriod(Date valuedate) {
    return (valuedate.serialNumber() > serialNumber) ? Date.DatesToPeriod(this, valuedate) : Date.DatesToPeriod(valuedate, this);
  }

  /**
   * Creates new period class as the difference between two dates
   *
   * @param target dates
   * @return new Period object
   */
  public static Period DatesToPeriod(
    Date from,
    Date to
  ) {
    int to_nbDaysInMonth = (new GregorianCalendar(to.year(), to.month().value(), to.dayOfMonth())).getActualMaximum(Calendar.DAY_OF_MONTH);

    if (from.year() == to.year() && from.month() == to.month() && from.dayOfMonth() == to.dayOfMonth()) {
      return new Period(0, TimeUnit.Days);
    }
    if (from.year() != to.year() && ((from.month() == to.month() && from.dayOfMonth() == to.dayOfMonth()) ||
      (from.month() == Month.February && from.dayOfMonth() == 29 && to.month() == Month.February && to.dayOfMonth() == 28 && to.dayOfMonth() == to_nbDaysInMonth))) {
      return new Period(to.year() - from.year(), TimeUnit.Years);
    } else if (from.month() != to.month() &&
      (from.dayOfMonth() == to.dayOfMonth() || (to.dayOfMonth() == to_nbDaysInMonth && from.dayOfMonth() > to.dayOfMonth()))) {
      return new Period(12 * (to.year() - from.year()) + (to.month().value() - from.month().value()), TimeUnit.Months);
    } else {
      int timespan = (int) (to.serialNumber() - from.serialNumber());
      return new Period(timespan, TimeUnit.Days);
    }
  }

  //
  // public inner classes
  //

  /**
   * This class provides a long output formatter, e.g: September 18, 2009
   * <p>
   * <b>NOTE</b>: Both java.util.Date and JQuantLib Date do not support time zones or high precision clocks.
   * In other words, a day <i>always</i> has exactly 84,600 seconds, or 84,600,000 milliseconds.
   */
  private final class LongDate extends java.util.Date {

    private static final long serialVersionUID = -8382775848256835100L;

    private LongDate() {
      super((serialNumber() - 25569) * 86400000L);
    }

    @Override
    public final String toString() {
      if (isNull())
        return "null date";
      else {
        final StringBuilder sb = new StringBuilder();
        final Formatter formatter = new Formatter(sb, Locale.US);
        formatter.format("%s %d, %d", month(), dayOfMonth(), year());
        return sb.toString();
      }
    }
  }


  /**
   * This class provides a short output formatter, e.g: 09/18/2009
   * <p>
   * <b>NOTE</b>: Both java.util.Date and JQuantLib Date do not support time zones or high precision clocks.
   * In other words, a day <i>always</i> has exactly 84,600 seconds, or 84,600,000 milliseconds.
   */
  private final class ShortDate extends java.util.Date {

    private static final long serialVersionUID = -4372510060405020533L;

    private ShortDate() {
      super((serialNumber() - 25569) * 86400000L);
    }

    @Override
    public final String toString() {
      if (isNull())
        return "null date";
      else {
        final StringBuilder sb = new StringBuilder();
        final Formatter formatter = new Formatter(sb, Locale.US);
        formatter.format("%02d/%02d/%4d", month().value(), dayOfMonth(), year());
        return sb.toString();
      }
    }
  }

  /**
   * This class provides an ISO date output formatter, e.g: 2009-09-18
   * <p>
   * <b>NOTE</b>: Both java.util.Date and JQuantLib Date do not support time zones or high precision clocks.
   * In other words, a day <i>always</i> has exactly 84,600 seconds, or 84,600,000 milliseconds.
   */
  private final class ISODate extends java.util.Date {

    private static final long serialVersionUID = 4824909887446169897L;

    private ISODate() {
      super((serialNumber() - 25569) * 86400000L);
    }

    @Override
    public final String toString() {
      if (isNull())
        return "null date";
      else {
        final StringBuilder sb = new StringBuilder();
        final Formatter formatter = new Formatter(sb, Locale.US);
        final Calendar c = Calendar.getInstance();
        c.setTime(this);
        formatter.format("%04d-%02d-%02d", c.get(Calendar.YEAR), c.get(Calendar.MONTH) + 1, c.get(Calendar.DAY_OF_MONTH));
        return sb.toString();
      }
    }
  }

}
