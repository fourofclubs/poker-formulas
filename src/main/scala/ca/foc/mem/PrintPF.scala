package ca.foc.mem

import java.io.{ File, PrintStream }

import ca.foc.mem.PokerFormulas.{ DealInfo, Flush, FullHouse, HandInfo, Quads, Straight }
import ca.foc.mem.PokerFormulas.StraightFlush
import ca.foc.mem.PokerFormulas.RoyalFlush

object PrintPF extends App {
  import PokerFormulas.seconds

  def fmt(h: HandInfo)(d: DealInfo) = s"${h.hand.toString}, ${h.players} : ${d.cut}-${d.seconds.mkString(",")}"

  val values = VALUES.toList.sorted
  
  val fhs = for (
    p <- 4 to 10;
    v1 <- values;
    v2 <- values
  ) yield {
    val hand = HandInfo(FullHouse(v1, v2), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val ss = for (
    p <- 4 to 10;
    v <- values
  ) yield {
    val hand = HandInfo(Straight(v), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val fs = for (
    p <- 4 to 10;
    s <- SUITS;
    v <- values
  ) yield {
    val hand = HandInfo(Flush(s, v), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val qs = for (
    p <- 4 to 10;
    v <- values
  ) yield {
    val hand = HandInfo(Quads(v), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val sfs = for (
    p <- 4 to 10;
    s <- SUITS;
    v <- values
  ) yield {
    val hand = HandInfo(StraightFlush(s, v), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val rfs = for (
    p <- 4 to 10;
    s <- SUITS
  ) yield {
    val hand = HandInfo(RoyalFlush(s), p)
    seconds.get(hand).map(fmt(hand)_).getOrElse("")
  }

  val pf = new File("pokerFormulas.csv")
  pf.delete()
  val ps = new PrintStream(pf)
  ps.println(fhs.mkString("\n\n") + "\n\n")
  ps.println(ss.mkString("\n\n") + "\n\n")
  ps.println(fs.mkString("\n\n") + "\n\n")
  ps.println(qs.mkString("\n\n") + "\n\n")
  ps.println(sfs.mkString("\n\n") + "\n\n")
  ps.println(rfs.mkString("\n\n") + "\n\n")

  println("DONE")
}