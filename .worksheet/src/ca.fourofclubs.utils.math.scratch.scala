package ca.fourofclubs.utils.math
import scala.collection.immutable.ListMap
import java.util.concurrent.TimeUnit
import org.joda.time.format.PeriodFormatter
import org.joda.time.format.ISOPeriodFormat
import org.joda.time.Duration
import org.joda.time.format.PeriodFormatterBuilder
import org.joda.time.format.PeriodFormat
import org.joda.time.Days

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(531); 
  implicit val units = ListMap[String, Double]("year(s)" -> 365, "month(s)" -> 30, "week(s)" -> 7,
    "day(s)" -> 1, "hour(s)" -> 0.0417, "minute(s)" -> 0.000694);System.out.println("""units  : scala.collection.immutable.ListMap[String,Double] = """ + $show(units ));$skip(436); 
  def formatDuration(duration: Double)(implicit units: ListMap[String, Double]): String = {
    def format(duration: Double, unit: List[(String, Double)]): String = unit match {
      case (unit, multiplier) :: rest ⇒ {
        val n = duration / multiplier
        (if (n.floor > 0) (n.toInt + " " + unit + " ") else "") + format(duration % multiplier, rest)
      }
      case List() ⇒ ""
    }
    format(duration, units.toList)
  };System.out.println("""formatDuration: (duration: Double)(implicit units: scala.collection.immutable.ListMap[String,Double])String""");$skip(41); val res$0 = 

  formatDuration(12.01)(units.take(4));System.out.println("""res0: String = """ + $show(res$0));$skip(46); 

  val duration = new Duration(20040000000L);System.out.println("""duration  : org.joda.time.Duration = """ + $show(duration ));$skip(16); val res$1 = 
  Days.days(25);System.out.println("""res1: org.joda.time.Days = """ + $show(res$1));$skip(54); val res$2 = 
  ISOPeriodFormat.standard().print(duration.toPeriod);System.out.println("""res2: String = """ + $show(res$2));$skip(51); val res$3 = 
  PeriodFormat.getDefault.print(duration.toPeriod);System.out.println("""res3: String = """ + $show(res$3));$skip(251); 
  val formatter = new PeriodFormatterBuilder().appendYears().appendSuffix("Years").appendMonths().appendSuffix("Months")
    .appendWeeks().appendSuffix("Weeks").appendDays().appendSuffix("Days").appendHours().appendSuffix("Hours")
    .toFormatter();System.out.println("""formatter  : org.joda.time.format.PeriodFormatter = """ + $show(formatter ));$skip(37); val res$4 = 
  formatter.print(duration.toPeriod);System.out.println("""res4: String = """ + $show(res$4));$skip(35); val res$5 = 
  formatter.print(Days.days(1525));System.out.println("""res5: String = """ + $show(res$5));$skip(251); 

  def isSafe(col: Int, queens: List[Int]): Boolean = queens match {
    case List()  => true
    case c :: cs => col != c && isSafe(col, cs) &&
    	col != c + 1 && isSafe(col, cs map (_ + 1)) &&
    	col != c - 1 && isSafe(col, cs map (_ - 1))
  };System.out.println("""isSafe: (col: Int, queens: List[Int])Boolean""");$skip(27); val res$6 = 
  
  isSafe(1, List(5, 2));System.out.println("""res6: Boolean = """ + $show(res$6))}
}
