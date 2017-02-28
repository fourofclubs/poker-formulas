package ca.foc.play

import java.text.NumberFormat

import scala.io.StdIn.readLine
import scala.math.min

import org.joda.time.DateTime
import org.joda.time.DateTimeConstants.FRIDAY
import org.joda.time.format.DateTimeFormat

import scalaz._
import scalaz.effect.IO
import scalaz.std.list.listInstance

object Mortgage extends App {
  val DF = DateTimeFormat.forPattern("dd-MMM-yyyy")
  val NF = NumberFormat.getCurrencyInstance
  val date = DateTime.now().withDayOfWeek(FRIDAY).minusWeeks(1)
  val interestRate = 0.0279

  def formatPayment(p: Payment) = s"${DF.print(p.date)}: Remaining: ${NF.format(p.remaining)}, " +
    s"Paid: ${NF.format(p.paid)}, Interest: ${NF.format(p.interest)}, Total Interest: ${NF.format(p.totalInterest)}"

  val io =
    for (
      initialPayment <- IO { readLine("Current Balance?").toDouble }.map(Payment.startingAt(date, _));
      payments <- IO { readLine("Weekly Payments?").toDouble }.map(initialPayment.subsequent(interestRate, _).toList);
      _ <- Traverse[List].traverse(payments)(p => IO { println(formatPayment(p)) });
      _ <- IO { println(s"${payments.tail.size} weeks left. Total interest: ${NF.format(payments.last.totalInterest)}") }
    ) yield ()

  io.unsafePerformIO()
}

case class Payment(date: DateTime, remaining: Double, paid: Double, interest: Double, totalInterest: Double) {
  def next(interestRate: Double, weeklyPayment: Double): Option[Payment] = if (this.remaining <= 0) None else {
    val interest = (this.remaining * interestRate) / 52.0
    val paid = this.paid + min(weeklyPayment, this.remaining + interest)
    val remaining = this.remaining - min(weeklyPayment - interest, this.remaining)
    Some(Payment(this.date.plusWeeks(1), remaining, paid, interest, this.totalInterest + interest))
  }
  def subsequent(interestRate: Double, weeklyPayment: Double): Stream[Payment] =
    Stream.iterate[Option[Payment]](Some(this))(_.flatMap(p => p.next(interestRate, weeklyPayment)))
      .takeWhile(_.isDefined).map(_.get)
}

object Payment {
  def startingAt(date: DateTime, start: Double) = Payment(date, start, 0, 0, 0)
}