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
  }

  val p1 = new Poly(1 -> 2.0, 2 -> 4.0, 5 -> 6.2) //> p1  : poly.Poly = 6.2x^5 + 4.0x^2 + 2.0x^1
  val p2 = new Poly(1 -> 1.0, 2 -> 3.1, 3 -> 4.2) //> p2  : poly.Poly = 4.2x^3 + 3.1x^2 + 1.0x^1
  p1 + p2                                         //> res0: poly.Poly = 6.2x^5 + 4.2x^3 + 7.1x^2 + 3.0x^1
}