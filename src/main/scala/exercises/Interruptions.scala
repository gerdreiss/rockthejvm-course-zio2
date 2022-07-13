package exercises

import zio.*

object Interruptions extends ZIOAppDefault:

  /**
   * 
   * if zio is successful before timeout => success
   * if zio fails before timeout => failure
   * if zio takes longer than timeout => interruption
   */
  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] =
    for
      fib <- zio.fork
      _   <- fib.interrupt.delay(time).fork
      a   <- fib.join
    yield a

  // return None when timed out
  def timeout2[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =
    // zio.timeout(time)
    timeout(zio, time)
      .foldCauseZIO(
        cause => if cause.isInterrupted then ZIO.succeed(None) else ZIO.failCause(cause),
        value => ZIO.succeed(Some(value))
      )

  val testTimeout: UIO[String] =
    timeout(
      ZIO
        .succeed("Not interrupted")
        .delay(2.seconds)
        .onInterrupt(ZIO.succeed("Interrupted").debug),
      1.second
    )

  val testTimeout2: UIO[Option[String]] =
    timeout2(
      ZIO
        .succeed("Not interrupted")
        .delay(1.seconds)
        .onInterrupt(ZIO.succeed("Interrupted").debug),
      1.second
    )

  override def run: ZIO[Any, Any, Any] =
    testTimeout2.debug
