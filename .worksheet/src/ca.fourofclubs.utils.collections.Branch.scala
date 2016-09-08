package ca.fourofclubs.utils.collections

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Leaf {
  def apply[A](value: A) = new Leaf(value)
}

object Trees {
}
