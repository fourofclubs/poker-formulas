package ca.fourofclubs.playground.monads

object mon {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(84); 
  val om = Monads.optionMonad;System.out.println("""om  : ca.fourofclubs.playground.monads.Mon[Option] = """ + $show(om ));$skip(27); val res$0 = 
  om.filterM(List(1,2,3))_;System.out.println("""res0: (Int => Option[Boolean]) => Option[List[Int]] = """ + $show(res$0));$skip(12); val res$1 = 
  "Testing";System.out.println("""res1: String("Testing") = """ + $show(res$1))}
}
