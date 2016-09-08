package utils
import Complex._

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(76); 
  val z = new Complex(1, 2);System.out.println("""z  : <error> = """ + $show(z ));$skip(29); 
  val w = new Complex(2, -1);System.out.println("""w  : <error> = """ + $show(w ));$skip(12); val res$0 = 
  z + 3 * w;System.out.println("""res0: <error> = """ + $show(res$0));$skip(9); val res$1 = 
  ~w - z;System.out.println("""res1: <error> = """ + $show(res$1));$skip(8); val res$2 = 
  z ^ 3;System.out.println("""res2: <error> = """ + $show(res$2));$skip(19); val res$3 = 
  ((w ^ 2) - w).re;System.out.println("""res3: <error> = """ + $show(res$3));$skip(19); val res$4 = 
  (z ^ 2) + ~z + i;System.out.println("""res4: <error> = """ + $show(res$4))}
}
