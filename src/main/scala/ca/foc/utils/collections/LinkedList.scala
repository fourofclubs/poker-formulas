package ca.foc.utils.collections
import java.util.List

trait LinkedList[+T] {
  def head: T
  def tail: LinkedList[T]
  def empty: Boolean
  def ::[E >: T](e: E): LinkedList[E] = new NonEmpty(e, this)
  def size: Int
  def apply(position: Int): T
  def +[E >: T](e: E): LinkedList[E]
  def -[E >: T](e: E): LinkedList[T]
}

object LinkedList {
  def apply[T]() = Nil
  def apply[T](head: T, tail: LinkedList[T]) = new NonEmpty(head, tail)
  def apply[T >: Any](es: Iterable[T]): LinkedList[T] = if (es.isEmpty) Nil else new NonEmpty(es.head, apply(es.tail))
}

class NonEmpty[+T](val head: T, val tail: LinkedList[T]) extends LinkedList[T] {
  val empty = false
  override def toString = "[" + head.toString + "," + tail.toString + "]"
  def size = 1 + tail.size
  def apply(position: Int) = if (position == 0) head else tail(position - 1)
  def +[E >: T](e: E): LinkedList[E] = head :: (tail + e)
  def -[E >: T](e: E): LinkedList[T] = if (head == e) tail else head :: (tail - e)
}

object Nil extends LinkedList[Any] {
  def head = throw new NoSuchElementException
  def tail = Nil
  val empty = true
  override def toString = "Nil"
  val size = 0
  def apply(position: Int) = throw new NoSuchElementException
  def +[E >: Any](e: E): LinkedList[E] = new NonEmpty(e, Nil)
  def -[E >: Any](e: E): LinkedList[Any] = Nil
}