package lessons

import utils.*
import zio.*

import scala.collection.immutable.AbstractSeq

object Parallelism extends ZIOAppDefault:

  val mol: UIO[Int]    = ZIO.succeed(42)
  val fav: UIO[String] = ZIO.succeed("🍎")

  // combine sequential effects
  val combined: UIO[(Int, String)] = mol.zip(fav)

  // combine in parallel
  val combinedParallel: UIO[(Int, String)] = mol.zipPar(fav)

  def customZipPar[R, E, A, B](
      fa: ZIO[R, E, A],
      fb: ZIO[R, E, B]
  ): ZIO[R, E, (A, B)] =
    val exits =
      for
        fibA  <- fa.fork
        fibB  <- fb.fork
        exitA <- fibA.await
        exitB <- exitA match
                   case Exit.Success(_) => fibB.await
                   case Exit.Failure(_) => fibB.interrupt
      yield (exitA, exitB)

    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b))   => ZIO.succeed((a, b))
      case (Exit.Failure(e), Exit.Success(_))   => ZIO.failCause(e)
      case (Exit.Success(_), Exit.Failure(e))   => ZIO.failCause(e)
      case (Exit.Failure(le), Exit.Failure(re)) => ZIO.failCause(le && re)
    }

  // parallel combinators
  // zipPar, zipWithPar
  // collectAllPar
  val effects: Seq[UIO[Int]]         = (1 to 10).map(ZIO.succeed(_).debugThread)
  val collectedValues: UIO[Seq[Int]] = ZIO.collectAllPar(effects) // "traverse"

  val printlnParallel: IO[java.io.IOException, Seq[Unit]] =
    ZIO.foreachPar(1 to 10)(Console.printLine(_))

  // reduceAllPar, mergeAllPar
  val reducePar: UIO[Int] =
    ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)

  val mergePar: UIO[Int] =
    ZIO.mergeAllPar(effects)(0)(_ + _)

  override def run: ZIO[Any, Any, Any] = mergePar.debugThread
