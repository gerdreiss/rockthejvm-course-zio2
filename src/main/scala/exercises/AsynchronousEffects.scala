package exercises

import utils.*
import zio.*

import java.util.concurrent.{ Executors, ExecutorService }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object AsynchronousEffects extends ZIOAppDefault:

  // 1. - surface a computation running on some (external) thread to a ZIO
  // hint 1: invoke the cb when the computation is done
  // hint 2: don't wrap the computation in a ZIO
  def externalToZIO[A](computation: () => A)(executor: ExecutorService): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      executor.execute { () =>
        Try(computation())
          .fold(
            e => cb(ZIO.fail(e)),
            a => cb(ZIO.succeed(a))
          )
      }
    }

  val demoExternalToZIO: Task[Unit] =
    externalToZIO { () =>
      println("computing the meaning of life on some thread".prependThreadName)
      Thread.sleep(1000)
      42
    }(Executors.newSingleThreadExecutor())
      .debugThread
      .unit

  // 2. - lift a Future to a ZIO
  // hint 1: invoke cb when the Future is done
  def futureToZIO[A](future: => Future[A])(using ExecutionContext): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      future.onComplete {
        case Success(a) => cb(ZIO.succeed(a))
        case Failure(e) => cb(ZIO.fail(e))
      }
    }

  lazy val demoFutureToZIO: Task[Unit] =
    given ExecutionContext =
      ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())

    futureToZIO {
      Future {
        println("computing the meaning of life on some thread".prependThreadName)
        Thread.sleep(1000)
        42
      }
    }.debugThread.unit

  def neverEndingZIO[A]: UIO[A] =
    ZIO.async(_ => ())

  def never: UIO[Nothing] = ZIO.never

  override def run: ZIO[Any, Any, Any] =
    ZIO.succeed("Computing the never ending ZIO...").debugThread *>
      neverEndingZIO[Int] *>
      ZIO.succeed("Completed.").debugThread
