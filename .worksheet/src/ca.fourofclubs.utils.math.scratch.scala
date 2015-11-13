package ca.fourofclubs.utils.math
import Complex._
import Math._
import Rational._
import Nat._

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(189); 
  val e1 = Prod(Sum(Val(1), Sum(Var("x"), Val(8))), Sum(Var("y"), Val(5)));System.out.println("""e1  : ca.fourofclubs.utils.math.Prod = """ + $show(e1 ));$skip(30); val res$0 = 
  e1.eval("x" -> 3, "y" -> 2);System.out.println("""res0: Double = """ + $show(res$0));$skip(21); val res$1 = 

  Nat("10") * "11";System.out.println("""res1: ca.fourofclubs.utils.math.Nat = """ + $show(res$1));$skip(20); val res$2 = 

  "199".successor;System.out.println("""res2: ca.fourofclubs.utils.math.Nat = """ + $show(res$2))}
}
