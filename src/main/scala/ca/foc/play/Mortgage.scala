package ca.foc.play

import java.text.NumberFormat

import scala.math._

import org.joda.time.DateTime
import org.joda.time.DateTimeConstants._
import org.joda.time.format.DateTimeFormat

object Mortgage extends App {
  val date = DateTime.now().withDayOfWeek(FRIDAY).minusWeeks(1)
  val start = 15485.02
  val interestRate = 0.0293

  val initial = Payment.startingAt(date, start)
  val payments1 = initial.subsequent(interestRate, 144)
  val payments2 = initial.subsequent(interestRate, 225)

  println({
    for ((p1, p2) <- payments1 zip payments2)
      yield s"${p1} ... ${p2} : ${p2.paid - p1.paid}"
  }.mkString("\n"))
}

case class Payment(date: DateTime, remaining: Double, paid: Double, interest: Double, totalInterest: Double) {
  private val DF = DateTimeFormat.forPattern("dd-MMM-yyyy")
  private val NF = NumberFormat.getCurrencyInstance
  override def toString = DF.print(date) + ": Remaining: " + NF.format(remaining) + ", Paid: " +
    NF.format(paid) + ", Interest: " + NF.format(interest) + ", Total Interest: " + NF.format(totalInterest)
  def next(interestRate: Double, weeklyPayment: Double) = Payment.next(interestRate, weeklyPayment)(this)
  def subsequent(interestRate: Double, weeklyPayment: Double): List[Payment] =
    Stream.iterate[Option[Payment]](Some(this))(_.flatMap(Payment.next(interestRate, weeklyPayment)))
      .takeWhile(_.isDefined).map(_.get).toList
}

object Payment {
  def startingAt(date: DateTime, start: Double) = Payment(date, start, 0, 0, 0)
  def next(interestRate: Double, weeklyPayment: Double)(p: Payment): Option[Payment] = if (p.remaining <= 0) None else {
    val interest = (p.remaining * interestRate) / 52.0
    val paid = p.paid + min(weeklyPayment, p.remaining + interest)
    val remaining = p.remaining - min(weeklyPayment - interest, p.remaining)
    Some(Payment(p.date.plusWeeks(1), remaining, paid, interest, p.totalInterest + interest))
  }
}