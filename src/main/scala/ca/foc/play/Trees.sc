package ca.fourofclubs.utils.collections

object Trees {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val testTree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(10)), Leaf(7)))
                                                  //> testTree  : ca.fourofclubs.utils.collections.Trees.Branch[Int] = Branch(Leaf
                                                  //| (1),Branch(Branch(Leaf(2),Leaf(10)),Leaf(7)))

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  }                                               //> size: [A](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])Int

  size(testTree)                                  //> res0: Int = 4

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(x)      => x
    case Branch(l, r) => max(l) max max(r)
  }                                               //> max: (tree: ca.fourofclubs.utils.collections.Trees.Tree[Int])Int
  max(testTree)                                   //> res1: Int = 10

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }                                               //> depth: [A](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])Int
  depth(testTree)                                 //> res2: Int = 3

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }                                               //> map: [A, B](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])(f: A => B)
                                                  //| ca.fourofclubs.utils.collections.Trees.Tree[B]
  map(testTree)(_ + 10)                           //> res3: ca.fourofclubs.utils.collections.Trees.Tree[Int] = Branch(Leaf(11),Bra
                                                  //| nch(Branch(Leaf(12),Leaf(20)),Leaf(17)))

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => fold(l, fold(r, z)(f))(f)
  }                                               //> fold: [A, B](tree: ca.fourofclubs.utils.collections.Trees.Tree[A], z: B)(f:
                                                  //|  (A, B) => B)B
  fold(testTree, 0)(_ + _)                        //> res4: Int = 20
  fold(testTree, 0)((a, b) => 1 + b)              //> res5: Int = 4
  fold(testTree, 1)(_ * _)                        //> res6: Int = 140
}