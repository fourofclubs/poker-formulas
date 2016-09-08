package ca.foc.play

package object random {
  type Rand[+A] = State[RNG, A]
}