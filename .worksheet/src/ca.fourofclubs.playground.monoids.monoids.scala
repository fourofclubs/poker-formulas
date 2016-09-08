package ca.fourofclubs.playground.monoids

import ca.fourofclubs.playground.testing.Gen

object monoids {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(113); val res$0 = 
	"Test";System.out.println("""res0: String("Test") = """ + $show(res$0));$skip(41); 

  val intAddition = Monoids.intAddition;System.out.println("""intAddition  : ca.fourofclubs.playground.monoids.Monoid[Int] = """ + $show(intAddition ))}
  //Monoids.monoidLaws(intAddition, Gen.choose(-10000, 10000))
}
