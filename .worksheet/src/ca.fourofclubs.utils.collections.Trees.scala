package ca.fourofclubs.utils.collections

object Trees {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(277); 

  val testTree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(10)), Leaf(7)));System.out.println("""testTree  : ca.fourofclubs.utils.collections.Trees.Branch[Int] = """ + $show(testTree ));$skip(125); 

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  };System.out.println("""size: [A](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])Int""");$skip(19); val res$0 = 

  size(testTree);System.out.println("""res0: Int = """ + $show(res$0));$skip(123); 

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(x)      => x
    case Branch(l, r) => max(l) max max(r)
  };System.out.println("""max: (tree: ca.fourofclubs.utils.collections.Trees.Tree[Int])Int""");$skip(16); val res$1 = 
  max(testTree);System.out.println("""res1: Int = """ + $show(res$1));$skip(136); 

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  };System.out.println("""depth: [A](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])Int""");$skip(18); val res$2 = 
  depth(testTree);System.out.println("""res2: Int = """ + $show(res$2));$skip(162); 

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  };System.out.println("""map: [A, B](tree: ca.fourofclubs.utils.collections.Trees.Tree[A])(f: A => B)ca.fourofclubs.utils.collections.Trees.Tree[B]""");$skip(24); val res$3 = 
  map(testTree)(_ + 10);System.out.println("""res3: ca.fourofclubs.utils.collections.Trees.Tree[Int] = """ + $show(res$3));$skip(162); 

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => fold(l, fold(r, z)(f))(f)
  };System.out.println("""fold: [A, B](tree: ca.fourofclubs.utils.collections.Trees.Tree[A], z: B)(f: (A, B) => B)B""");$skip(27); val res$4 = 
  fold(testTree, 0)(_ + _);System.out.println("""res4: Int = """ + $show(res$4));$skip(37); val res$5 = 
  fold(testTree, 0)((a, b) => 1 + b);System.out.println("""res5: Int = """ + $show(res$5));$skip(27); val res$6 = 
  fold(testTree, 1)(_ * _);System.out.println("""res6: Int = """ + $show(res$6))}
}
