package ca.fourofclubs.utils.collections

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Trees {
  val testTree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(10)), Leaf(7)))

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  }
  
  size(testTree)
}
