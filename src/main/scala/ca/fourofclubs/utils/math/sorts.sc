package ca.fourofclubs.utils.math

object sorts {
  val test1 = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
  val test2 = List()
  val test3 = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val test4 = List(3, 5, 6, 4, 3, 8, 7, 8, 9, 19, 87, 3, 4)

  def insertionSort(xs: List[Int], op: () ⇒ Unit): List[Int] = {
    op()
    xs match {
      case List()  ⇒ List()
      case y :: ys ⇒ insert(y, insertionSort(ys, op), op)
    }
  }
  def insert(x: Int, xs: List[Int], op: () ⇒ Unit): List[Int] = {
    op()
    xs match {
      case List()  ⇒ List(x)
      case y :: ys ⇒ if (x < y) x :: xs else y :: insert(x, ys, op)
    }
  }
	val f = insertionSort_
  def quickSort(xs: List[Int], op: () ⇒ Unit): List[Int] = {
    op()
    xs match {
      case List()  ⇒ List()
      case y :: ys ⇒ quickSort(filter(ys, _ <= y, op), op) ::: y :: quickSort(filter(ys, _ > y, op), op)
    }
  }
  def filter(xs: List[Int], p: Int ⇒ Boolean, op: () ⇒ Unit): List[Int] = {
    op()
    xs match {
      case List()  ⇒ List()
      case y :: ys ⇒ if (p(y)) y :: filter(ys, p, op) else filter(ys, p, op)
    }
  }
  def mergeSort(xs: List[Int], op: () ⇒ Unit): List[Int] = {
    op()
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (begin, end) = xs splitAt n
      merge(mergeSort(begin, op), mergeSort(end, op), op)
    }
  }
  def merge(xs: List[Int], ys: List[Int], op: () ⇒ Unit): List[Int] = {
    op()
    (xs, ys) match {
      case (List(), ys1)        ⇒ ys1
      case (xs1, List())        ⇒ xs1
      case (x :: xs1, y :: ys1) ⇒ if (x < y) x :: merge(xs1, ys, op) else y :: merge(xs, ys1, op)
    }
  }

  def compare(xs: List[Int]) = {
    var n = 0
    println(insertionSort(xs, () ⇒ n += 1))
    println("Insertion Sort: " + n)
    n = 0
    println(quickSort(xs, () ⇒ n += 1))
    println("Quick Sort: " + n)
    n = 0
    println(mergeSort(xs, () ⇒ n += 1))
    println("Merge Sort: " + n)
  }
  compare(test1)
  compare(test2)
  compare(test3)
  compare(test4)
}