package ca.fourofclubs.playground.monads

object mons {
  implicit val om = Monads.optionMonad
  val lt = Traverse.listTraverse
  val lm = List(om, om)
  lt.sequence[Option, Int](List(Some(1), Some(2)))
}