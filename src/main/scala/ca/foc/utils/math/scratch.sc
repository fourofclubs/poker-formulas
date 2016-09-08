package ca.fourofclubs.utils.math
import scala.collection.immutable.ListMap
import java.util.concurrent.TimeUnit
import org.joda.time.format.PeriodFormatter
import org.joda.time.format.ISOPeriodFormat
import org.joda.time.Duration
import org.joda.time.format.PeriodFormatterBuilder
import org.joda.time.format.PeriodFormat
import org.joda.time.Days

object scratch {
  implicit val units = ListMap[String, Double]("year(s)" -> 365, "month(s)" -> 30, "week(s)" -> 7,
    "day(s)" -> 1, "hour(s)" -> 0.0417, "minute(s)" -> 0.000694)
                                                  //> units  : scala.collection.immutable.ListMap[String,Double] = Map(year(s) -> 
                                                  //| 365.0, month(s) -> 30.0, week(s) -> 7.0, day(s) -> 1.0, hour(s) -> 0.0417, m
                                                  //| inute(s) -> 6.94E-4)
  def formatDuration(duration: Double)(implicit units: ListMap[String, Double]): String = {
    def format(duration: Double, unit: List[(String, Double)]): String = unit match {
      case (unit, multiplier) :: rest ⇒ {
        val n = duration / multiplier
        (if (n.floor > 0) (n.toInt + " " + unit + " ") else "") + format(duration % multiplier, rest)
      }
      case List() ⇒ ""
    }
    format(duration, units.toList)
  }                                               //> formatDuration: (duration: Double)(implicit units: scala.collection.immutabl
                                                  //| e.ListMap[String,Double])String

  formatDuration(12.01)(units.take(4))            //> res0: String = "1 week(s) 5 day(s) "

  val duration = new Duration(20040000000L)       //> duration  : org.joda.time.Duration = PT20040000S
  Days.days(25)                                   //> res1: org.joda.time.Days = P25D
  ISOPeriodFormat.standard().print(duration.toPeriod)
                                                  //> res2: String = PT5566H40M
  PeriodFormat.getDefault.print(duration.toPeriod)//> res3: String = 5566 hours and 40 minutes
  val formatter = new PeriodFormatterBuilder().appendYears().appendSuffix("Years").appendMonths().appendSuffix("Months")
    .appendWeeks().appendSuffix("Weeks").appendDays().appendSuffix("Days").appendHours().appendSuffix("Hours")
    .toFormatter()                                //> formatter  : org.joda.time.format.PeriodFormatter = org.joda.time.format.Pe
                                                  //| riodFormatter@19aa19aa
  formatter.print(duration.toPeriod)              //> res4: String = 5566Hours
  formatter.print(Days.days(1525))                //> res5: String = 1525Days

  def isSafe(col: Int, queens: List[Int]): Boolean = queens match {
    case List()  => true
    case c :: cs => col != c && isSafe(col, cs) &&
    	col != c + 1 && isSafe(col, cs map (_ + 1)) &&
    	col != c - 1 && isSafe(col, cs map (_ - 1))
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean
  
  isSafe(1, List(5, 2))                           //> res6: Boolean = false
}