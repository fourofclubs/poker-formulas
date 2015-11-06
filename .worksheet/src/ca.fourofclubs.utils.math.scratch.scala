package ca.fourofclubs.utils.math
import Complex._
import Math._
import Rational._

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(116); val res$0 = 
  Complex(1, 2);System.out.println("""res0: ca.fourofclubs.utils.math.Complex = """ + $show(res$0));$skip(27); 
  val one = Zero.successor;System.out.println("""one  : ca.fourofclubs.utils.math.Nat = """ + $show(one ));$skip(22); 
  val two = one + one;System.out.println("""two  : ca.fourofclubs.utils.math.Nat = """ + $show(two ));$skip(24); 
  val three = two + one;System.out.println("""three  : ca.fourofclubs.utils.math.Nat = """ + $show(three ));$skip(13); val res$1 = 
  Zero * two;System.out.println("""res1: ca.fourofclubs.utils.math.Nat = """ + $show(res$1));$skip(18); val res$2 = 
  two * two + one;System.out.println("""res2: ca.fourofclubs.utils.math.Nat = """ + $show(res$2));$skip(13); val res$3 = 
  two == one;System.out.println("""res3: Boolean = """ + $show(res$3));$skip(13); val res$4 = 
  one == one;System.out.println("""res4: Boolean = """ + $show(res$4));$skip(24); val res$5 = 
  one == Zero.successor;System.out.println("""res5: Boolean = """ + $show(res$5));$skip(13); val res$6 = 
  one < Zero;System.out.println("""res6: Boolean = """ + $show(res$6));$skip(13); val res$7 = 
  one > Zero;System.out.println("""res7: Boolean = """ + $show(res$7));$skip(13); val res$8 = 
  two >= two;System.out.println("""res8: Boolean = """ + $show(res$8));$skip(13); val res$9 = 
  one <= two;System.out.println("""res9: Boolean = """ + $show(res$9))}
}
