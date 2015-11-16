package ca.fourofclubs.utils.math
import Nat._

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(84); val res$0 = 
  Nat("10") * "11";System.out.println("""res0: ca.fourofclubs.utils.math.Nat = """ + $show(res$0));$skip(21); val res$1 = 

  string2Nat("hA");System.out.println("""res1: ca.fourofclubs.utils.math.Nat = """ + $show(res$1));$skip(13); val res$2 = 
  Spring + 5;System.out.println("""res2: <error> = """ + $show(res$2))}
}

sealed trait Season {
  def +(n: Int): Season = if (n > 0) next + (n - 1) else if (n < 0) previous + (n + 1) else this
  def -(n: Int) = this + (-n)
  def next: Season
  def previous: Season
}
case object Spring extends Season { def next = Summer; def previous = Winter }
case object Summer extends Season { def next = Autumn; def previous = Spring }
case object Autumn extends Season { def next = Winter; def previous = Summer }
case object Winter extends Season { def next = Spring; def previous = Autumn }
