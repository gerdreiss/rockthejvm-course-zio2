package exercises

import utils.*
import zio.*

object Promises extends ZIOAppDefault:

  // 1. Write a simulated "egg boiler" with two ZIOs
  //    - one increments a counter every 1s
  //    - one waits for the counter ot become 10, after which it will "ring a bell"
  def eggBoiler(): UIO[Unit] =
    def eggReady(signal: Promise[Nothing, Unit]): UIO[Unit] =
      for
        _ <- ZIO.succeed("Egg boiling...").debugThread
        _ <- signal.await
        _ <- ZIO.succeed("Egg ready!").debugThread
      yield ()

    def tickingClockD(ticks: Ref[Int], signal: Promise[Nothing, Unit]): UIO[Unit] =
      for
        _     <- ZIO.sleep(1.second)
        count <- ticks.updateAndGet(_ + 1)
        _     <- ZIO.succeed(count).debugThread
        _     <- if count == 10 then signal.succeed(()) else tickingClockD(ticks, signal)
      yield ()

    def tickingClockG(ticks: Ref[Int], signal: Promise[Nothing, Unit]): UIO[Unit] =
      for
        _ <- ticks
               .updateAndGet(_ + 1)
               .delay(1.second)
               .debugThread("Count: ")
               .repeatUntil(_ == 10)
        _ <- signal.succeed(())
      yield ()

    for
      ticks  <- Ref.make(0)
      signal <- Promise.make[Nothing, Unit]
      _      <- tickingClockG(ticks, signal) <&> eggReady(signal)
    yield ()

  // 2. Write a "race pair"
  //    - use a Promise which can hold an Either[exit for A, exit for B]
  //    - start a fiber for ZIO
  //    - on completion (with any status), each ZIO needs to complete that Promise
  //      (hint: use a finalizer)
  //    - waiting on the Promise's value can be interrupted!
  //    - if the whole race is interrupted, interrupt the running fibers
  def racePair[R, E, A, B](
      fa: => ZIO[R, E, A],
      fb: => ZIO[R, E, B]
  ): URIO[R, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] =
    ZIO.uninterruptibleMask { restore =>
      for
        promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]

        fibA <- fa.onExit(exitA => promise.succeed(Left(exitA))).fork
        fibB <- fb.onExit(exitB => promise.succeed(Right(exitB))).fork

        result <- restore(promise.await).onInterrupt(fibA.interrupt <&> fibB.interrupt)
      yield result match
        case Left(value)  => Left((value, fibB))
        case Right(value) => Right((fibA, value))
    }

  override def run: ZIO[Any, Any, Any] = eggBoiler()
