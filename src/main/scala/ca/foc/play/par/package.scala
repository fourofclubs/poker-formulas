package ca.fourofclubs.playground

import java.util.concurrent.{ ExecutorService, Future }

package ca.foc.play.par par {
  type Par[A] = ExecutorService => Future[A]
}