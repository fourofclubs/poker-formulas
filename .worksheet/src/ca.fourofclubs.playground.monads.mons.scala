package ca.fourofclubs.playground.monads

object mons {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(95); 
  implicit val om = Monads.optionMonad;System.out.println("""om  : ca.fourofclubs.playground.monads.Monad[Option] = """ + $show(om ));$skip(33); 
  val lt = Traverse.listTraverse;System.out.println("""lt  : ca.fourofclubs.playground.monads.Traverse[List] = """ + $show(lt ));$skip(24); 
  val lm = List(om, om);System.out.println("""lm  : List[ca.fourofclubs.playground.monads.Monad[Option]] = """ + $show(lm ));$skip(51); val res$0 = 
  lt.sequence[Option, Int](List(Some(1), Some(2)));System.out.println("""res0: Option[List[Int]] = """ + $show(res$0))}
}
