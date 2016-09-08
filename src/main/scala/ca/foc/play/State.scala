package ca.foc.play

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    g(a).run(s2)
  })
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def times(n: Int): State[S, Seq[A]] = State.sequence(Seq.fill(n)(this))
  def apply(s: S) = run(s)
}
object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))
  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    ra.flatMap(a => rb.flatMap(b => State.unit(f(a, b))))
  def both[S, A, B](ra: State[S, A], rb: State[S, B]): State[S, (A, B)] = map2(ra, rb)((_, _))
  def sequence[S, A](fs: Seq[State[S, A]]): State[S, Seq[A]] = fs match {
    case Seq() => State(s => (List[A](), s))
    case head :: tail => State(s => {
      val (a, s2) = head.run(s)
      val (as, s3) = sequence(tail).run(s2)
      (a +: as, s3)
    })
  }
}