package ca.foc.mem

object ParTest extends App {
  val ps = for (p <- (4 to 10).par) yield {
    println(p)
    p + 4
  }
  println(ps)
}