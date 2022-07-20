package lessons

import utils.*
import zio.*

object Semaphores extends ZIOAppDefault:

  // n permits
  // acquire, acquireN - can potentially (semantically) block the fiber
  // release, releaseN

  // example: limiting the number of concurrent sessions on a server

  // Semaphore.make(1) => a Mutex

  val semaphore: UIO[Semaphore] = Semaphore.make(10)

  def doWorkWhileLoggedIn(): UIO[Int] =
    Random.nextIntBounded(100).delay(1.second)

  private def loginProgram(id: Int) =
    for
      _      <- ZIO.succeed(s"[Task $id] logging in, working...").debugThread
      // critical section start
      result <- doWorkWhileLoggedIn()
      // critical section end
      _      <- ZIO.succeed(s"[Task $id] done: $result").debugThread
    yield result

  def login(id: Int, sem: Semaphore): UIO[Int] =
    ZIO.succeed(s"[Task $id] waiting for log in").debugThread *>
      sem.withPermit {
        loginProgram(id)
      }

  def demoSemaphore(): Task[Unit] =
    for
      sem <- Semaphore.make(2)
      f1  <- login(1, sem).fork
      f2  <- login(2, sem).fork
      f3  <- login(3, sem).fork
      _   <- f1.join *> f2.join *> f3.join
    yield ()

  def loginWeighted(n: Int, sem: Semaphore): UIO[Int] =
    ZIO.succeed(s"[Task $n] waiting for log in with $n permits").debugThread *>
      sem.withPermits(n) {
        loginProgram(n)
      }

  def demoSemaphoreWeighted(): Task[Unit] =
    for
      sem <- Semaphore.make(2)
      // requires 1 permit
      f1  <- loginWeighted(1, sem).fork
      // requires 2 permits
      f2  <- loginWeighted(2, sem).fork
      // requires 3 permits - will block because of the semaphore with 2 permits
      f3  <- loginWeighted(3, sem).fork
      _   <- f1.join *> f2.join *> f3.join
    yield ()

  override def run: ZIO[Any, Any, Any] = demoSemaphoreWeighted()
