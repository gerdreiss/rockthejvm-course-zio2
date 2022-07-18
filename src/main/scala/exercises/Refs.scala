package exercises

import utils.*
import zio.*

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault:

  def tickingClock(ticks: Ref[Int]): Task[Unit] =
    Console.print(".") *> ticks.update(_ + 1) *> tickingClock(ticks).delay(1.second)

  def printTicks(ticks: Ref[Int]): UIO[Unit] =
    ticks.get.delay(5.seconds).debugThread("Ticks: ") *> printTicks(ticks)

  val program: Task[Unit] =
    for
      ticks <- Ref.make(0)
      // fib2  <- printTicks(ticks).fork
      // fib1  <- tickingClock(ticks).fork
      // _     <- fib1.join
      // _     <- fib2.join
      _     <- tickingClock(ticks) <&> printTicks(ticks)
    yield ()

  override def run: Task[Unit] =
    Ref.make(0).flatMap(ticks => tickingClock(ticks) <&> printTicks(ticks))
