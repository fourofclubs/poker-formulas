package ca.foc.mem

import ca.foc.mem.PokerFormulas.HandInfo
import ca.foc.mem.PokerFormulas.DealInfo
import ca.foc.mem.PokerFormulas.FullHouse
import java.io.File
import java.io.PrintStream

object PrintPF extends App {
  import PokerFormulas.seconds

  def fmt(h: HandInfo)(d: DealInfo) = s"${h.hand.toString}, ${h.players} : ${d.cut}-${d.seconds.mkString(",")}"

  val sortedValues = VALUES.toList.sortBy(v ⇒ if (v == A) 14 else v.intVal).reverse
  val t = for (v1 ← sortedValues) yield {
    for (p ← 3 to 10) yield {
      for (
        v2 ← sortedValues.filterNot(_ == v1)
      ) yield {
        val h = HandInfo(FullHouse(v1, v2), p)
        seconds.get(h).map(fmt(h)_).getOrElse("")
      }
    }.mkString("\t")
  }.mkString("\n")

  val fh = new File("pokerFormulas-FH.csv")
  fh.delete()
  val pfh = new PrintStream(fh)
  pfh.println(t.mkString("\n\n"))

  for ((h, d) <- seconds.toList.sortBy(_._1.players).sortBy(_._1.hand.toString))
    println(h.hand.toString + ", " + h.players + ": " + d.cut + "-" + d.seconds.mkString(","))
}