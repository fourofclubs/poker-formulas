package utils
import Complex._

object scratch {
  val z = new Complex(1, 2)                       //> z  : utils.Complex = 1+2i
  val w = new Complex(2, -1)                      //> w  : utils.Complex = 2-i
  z + 3 * w                                       //> res0: utils.Complex = 7.0-i
  ~w - z                                          //> res1: utils.Complex = 1-i
  z ^ 3                                           //> res2: utils.Complex = -11-2i
  ((w ^ 2) - w).re                                //> res3: BigDecimal = 1
  (z ^ 2) + ~z + i                                //> res4: utils.Complex = -2+3i
}