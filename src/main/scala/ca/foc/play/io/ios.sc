package ca.foc.play.io

object ios {
  for {
    r1 <- STRef[Nothing, Int](1);
    r2 <- STRef[Nothing, Int](1);
    x <- r1.read;
    y <- r2.read;
    _ <- r1.write(y + 1);
    _ <- r2.write(x + 1);
    a <- r1.read;
    b <- r2.read
  } yield (a, b)                                  //> res0: ca.foc.play.io.ST[Nothing,(Int, Int)] = ca.foc.play.io.package$ST$$ano
                                                  //| n$6@1a93a7ca

  def p(n: Int, m: Int) = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(n);
      r2 <- STRef(m);
      x <- r1.read;
      y <- r2.read;
      _ <- r1.write(y + 1);
      _ <- r2.write(x + 1);
      a <- r1.read;
      b <- r2.read
    } yield (a, b)
  }                                               //> p: (n: Int, m: Int)ca.foc.play.io.RunnableST[(Int, Int)]
  ST.runST(p(1, 2))                               //> res1: (Int, Int) = (3,2)

  def p2 = new RunnableST[List[String]] {
    def apply[S] = for {
      a <- STArray(6, "Test");
      _ <- a.write(4, "Testing");
      _ <- a.swap(2, 4);
      r <- a.freeze
    } yield r
  }                                               //> p2: => ca.foc.play.io.RunnableST[List[String]]
  ST.runST(p2)                                    //> res2: List[String] = List(Test, Test, Testing, Test, Test, Test)

  def noop[S] = ST[S, Unit](())                   //> noop: [S]=> ca.foc.play.io.ST[S,Unit]
  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- a.read(i)
      _ <- if (vi < vp) (for {
        vj <- j.read
        _ <- a.swap(i, vj)
        _ <- j.write(vj + 1)
      } yield ())
      else noop[S]
    } yield ())
    x <- j.read
    _ <- a.swap(x, r)
  } yield x                                       //> partition: [S](a: ca.foc.play.io.STArray[S,Int], l: Int, r: Int, pivot: Int
                                                  //| )ca.foc.play.io.ST[S,Int]

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield ()
  else noop[S]                                    //> qs: [S](a: ca.foc.play.io.STArray[S,Int], l: Int, r: Int)ca.foc.play.io.ST[
                                                  //| S,Unit]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })                                            //> quicksort: (xs: List[Int])List[Int]
    
  quicksort(List(2,4,6,10,12,4,5,3,9,4,21,5))     //> res3: List[Int] = List(2, 3, 4, 4, 4, 5, 5, 6, 9, 10, 12, 21)
}