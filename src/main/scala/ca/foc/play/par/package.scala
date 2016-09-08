package ca.foc.play

import java.util.concurrent.{ ExecutorService, Future }

package object par {
  type Par[A] = ExecutorService => Future[A]
}