package ca.fourofclubs.playground.monads

object mons {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(95); 
  implicit val om = Monads.optionMonad;System.out.println("""om  : <error> = """ + $show(om ));$skip(33); 
  val lt = Traverse.listTraverse;System.out.println("""lt  : ca.fourofclubs.playground.monads.Traverse[List] = """ + $show(lt ));$skip(24); 
  val lm = List(om, om);System.out.println("""lm  : <error> = """ + $show(lm ));$skip(51); val res$0 = 
  lt.sequence[Option, Int](List(Some(1), Some(2)));System.out.println("""res0: <error> = """ + $show(res$0))}
}
