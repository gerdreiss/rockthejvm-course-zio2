package lessons

import utils.*
import zio.*

object Fibers extends ZIOAppDefault:

  val meaningOfLife: UIO[Int] = ZIO.succeed(42)
  val favLang: UIO[String]    = ZIO.succeed("Scala")

  def createFiber: Fiber[Throwable, String] = ??? // impossible

  val sameThreadIO: UIO[(Int, String)] =
    for
      mol  <- meaningOfLife.debugThread
      lang <- favLang.debugThread
    yield (mol, lang)

  val fiberIO: UIO[(Int, String)] =
    for
      fmol  <- meaningOfLife.debugThread.fork
      flang <- favLang.debugThread.fork
      mol   <- fmol.join
      lang  <- flang.join
    yield (mol, lang)

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] =
    meaningOfLife.fork

  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    for
      fib <- zio.fork
      a   <- fib.join
    yield a

  // awaiting a fiber
  def runOnAnotherThread2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, String] =
    for
      fib <- zio.fork
      a   <- fib.await
    yield a match
      case Exit.Success(value) => s"succeeded with $value"
      case Exit.Failure(cause) => s"failed with $cause"

  // peek at the result
  val peekFiber: UIO[Option[Exit[Nothing, Int]]] =
    for
      fib <- meaningOfLife.delay(1.second).fork
      a   <- fib.poll
    yield a

  // compose fibers
  val zippedFibers: UIO[(Int, String)] =
    for
      fib1   <- meaningOfLife.debugThread.fork
      fib2   <- favLang.debugThread.fork
      fiber   = fib1.zip(fib2)
      result <- fiber.join
    yield result

  // orElse
  val chainedFibers: UIO[String] =
    for
      fib1   <- ZIO.fail("error").debugThread.fork
      fib2   <- ZIO.succeed("success").debugThread.fork
      fiber   = fib1 <> fib2
      result <- fiber.join
    yield result

  def run = chainedFibers.debugThread
