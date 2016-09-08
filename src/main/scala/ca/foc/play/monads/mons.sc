package ca.fourofclubs.playground.monads

object mons {
  implicit val om = Monad.optionMonad
  val lt = Traverse.listTraverse
  val lm = List(om, om)
  lt.sequence[Option, Int](List(Some(1), Some(2)))
  val t = lt.traverse[Option, Int, Int](List(Some(1), Some(2))(x => Some(x+3))(om)
}