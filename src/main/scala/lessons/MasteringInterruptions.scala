package lessons

import utils.*
import zio.*

object MasteringInterruptions extends ZIOAppDefault:

  val aManuallyInterruptedZIO: Task[String] =
    ZIO.succeed("computing...").debugThread *>
      ZIO.interrupt *>
      ZIO.succeed("computed").debugThread

  val anEffectWithInterruptionFinalizer: Task[String] =
    aManuallyInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted").debugThread)

  // uninterruptible effect
  // payment flow to NOT be interrupted
  val fussyPaymentSystem: UIO[String] =
    (
      ZIO.succeed("payment running, don't cancel").debugThread *>
        ZIO.sleep(1.second) *>                                   // the actual payment flow
        ZIO.succeed("payment completed").debugThread
    )
      .onInterrupt(ZIO.succeed("payment cancelled").debugThread) // don't want this triggered

  val cancellationOfDoom: Task[Unit] =
    for
      fib <- fussyPaymentSystem.fork
      _   <- fib.interrupt.delay(500.millis)
      _   <- fib.join
    yield ()

  // ZIO.uninterruptible(cancellationOfDoom)
  val atomicPayment: UIO[String] =
    ZIO.uninterruptible(fussyPaymentSystem)

  val noCancellationOfDoom: Task[Unit] =
    for
      fib <- atomicPayment.fork
      _   <- fib.interrupt.delay(500.millis)
      _   <- fib.join
    yield ()

  // interruptibility is regional
  val zio1: UIO[Int]         = ZIO.succeed(1)
  val zio2: UIO[Int]         = ZIO.succeed(2)
  val zio3: UIO[Int]         = ZIO.succeed(3)
  val zioComposed: UIO[Int]  = (zio1 *> zio2 *> zio3).uninterruptible
  val zioComposed2: UIO[Int] = (zio1 *> zio2.interruptible *> zio3).interruptible

  // uninterruptibleMask

  /** example: an auth service
   * - input password, can be interrupted, because otherwise it might block the fiber indefinitely
   * - verify password, cannot be interrupted once it's triggered 
   */
  val inputPassword: UIO[String] =
    for
      _    <- ZIO.succeed("Input password: ").debugThread
      _    <- ZIO.succeed("(typing password)").debugThread
      _    <- ZIO.sleep(2.seconds)
      pass <- ZIO.succeed("password").debugThread
    yield pass

  def verifyPassword(pw: String): UIO[Boolean] =
    for
      _      <- ZIO.succeed("Verifying password...").debugThread
      _      <- ZIO.sleep(2.seconds)
      result <- ZIO.succeed(pw == "password").debugThread
    yield result

  val authFlow: UIO[Unit] =
    ZIO.uninterruptibleMask { r =>
      for
        password <- r(inputPassword)         // this is interruptible due to wrapping the effect in r
                      .onInterrupt(ZIO.succeed("Auth cancelled").debugThread)
        verified <- verifyPassword(password) // this is not interruptible
        _        <- if verified then ZIO.succeed("Auth successful").debugThread
                    else ZIO.succeed("Auth failed").debugThread
      yield ()
    }

  val authProgram: UIO[Unit] =
    for
      authFib <- authFlow.fork
      _       <- ZIO
                   .succeed("Attempting to cancel the auth flow")
                   .debugThread
                   .delay(3.seconds) *> authFib.interrupt
      _       <- authFib.join
    yield ()

  override def run: ZIO[Any, Any, Any] = authProgram
