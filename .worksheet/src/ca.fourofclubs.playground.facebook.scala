package ca.fourofclubs.playground

import facebook4j.FacebookFactory

object Facebook {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(142); 
  val facebook = new FacebookFactory().getInstance();System.out.println("""facebook  : facebook4j.Facebook = """ + $show(facebook ))}
}
