package lessons

import zio.*

object Interruptions extends ZIOAppDefault:
  val zioWithTime: UIO[Int] =
    ZIO.succeed("starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
      ZIO.succeed(42).debugThread

  val zioWithTimeOnInterrupt: UIO[Int] =
    zioWithTime.onInterrupt(ZIO.succeed("I was interrupted").debugThread)

  val interruption: UIO[Int] =
    for
      fib    <- zioWithTime.fork
      _      <- ZIO.succeed("Interrupting!").delay(1.second) *> fib.interrupt
      _      <- ZIO.succeed("Interruption successful").debugThread
      result <- fib.join
    yield result

  val interruption2: UIO[Int] =
    for
      fib    <- zioWithTimeOnInterrupt.fork
      _      <- ZIO.succeed("Interrupting!").delay(1.second) *> fib.interruptFork
      _      <- ZIO.succeed("Interruption successful").debugThread
      result <- fib.join
    yield result

  // automatic interruption
  // outliving a parent fiber
  val parentEffect: UIO[String] =
    ZIO.succeed("parent spawning fiber").debugThread *>
      // zioWithTimeOnInterrupt.fork *> // this will be interrupted by the parent fiber
      zioWithTimeOnInterrupt.forkDaemon *>         // this won't be interrupted by the parent fiber
      ZIO.sleep(1.second) *>
      ZIO.succeed("parent successful").debugThread // done

  val testOutlivingParent: UIO[Unit] =
    for
      fib <- parentEffect.fork
      _   <- ZIO.sleep(3.seconds)
      _   <- fib.join
    yield ()

  // racing
  val slowEffect: UIO[String] =
    ZIO
      .succeed("slow")
      .debugThread
      .delay(2.seconds)
      .onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)

  val fastEffect: UIO[String] =
    ZIO
      .succeed("fast")
      .debugThread
      .delay(1.seconds)
      .onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)

  val winner: UIO[ String] = slowEffect.race(fastEffect)

  override def run: ZIO[Any, Any, Any] = winner
