package exercises

import utils.*
import zio.*

import scala.collection.immutable.AbstractSeq

object Semaphores extends ZIOAppDefault:

  val tasks: UIO[Seq[Int]] =
    Semaphore.make(1)
      .flatMap { semaphore =>
        ZIO.foreachPar(1 to 10) { id =>
          for
            _      <- ZIO.succeed(s"[Task $id] waiting for log in").debugThread
            result <- semaphore.withPermit { // acquire + effect + release
                        for
                          _ <- ZIO.succeed(s"[Task $id] logged in, working...").debugThread
                          // critical section start
                          i <- Random.nextIntBounded(100).delay(1.second)
                          // critical section end
                          _ <- ZIO.succeed(s"[Task $id] done: $i").debugThread
                        yield i
                      }
          yield result
        }
      }

  override def run: ZIO[Any, Any, Any] = tasks
