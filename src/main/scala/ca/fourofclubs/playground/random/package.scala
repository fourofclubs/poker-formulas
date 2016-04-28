package ca.fourofclubs.playground

package object random {
  type Rand[+A] = State[RNG, A]
}