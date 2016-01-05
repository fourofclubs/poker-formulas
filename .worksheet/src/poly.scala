import scala.collection.immutable.Map.WithDefault

object poly {
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly) = new Poly((other.terms foldLeft this.terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      terms + (exp -> (terms(exp) + coeff))
    }
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(743); 

  val p1 = new Poly(1 -> 2.0, 2 -> 4.0, 5 -> 6.2);System.out.println("""p1  : poly.Poly = """ + $show(p1 ));$skip(50); 
  val p2 = new Poly(1 -> 1.0, 2 -> 3.1, 3 -> 4.2);System.out.println("""p2  : poly.Poly = """ + $show(p2 ));$skip(10); val res$0 = 
  p1 + p2;System.out.println("""res0: poly.Poly = """ + $show(res$0))}
}
