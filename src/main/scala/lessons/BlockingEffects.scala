package lessons

import utils.*
import zio.*

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable.AbstractSeq

object BlockingEffects extends ZIOAppDefault:

  def blockingTask(n: Int): Task[Unit] =
    Console.printLine(s"running blocking task $n") *>
      ZIO.sleep(10.seconds) *>
      blockingTask(n)

  val program: Task[Unit] =
    ZIO.foreachParDiscard(1 to 1000)(blockingTask)

  // blocking thread pool
  val aBlockingZIO: Task[Int] =
    ZIO.attemptBlocking {
      println("running a long computation...".prependThreadName)
      Thread.sleep(10000)
      42
    }

  // can use attemptBlockingInterrupt
  val aBlockingInterruptibleZIO: Task[Int] =
    ZIO.attemptBlockingInterrupt {
      println("running a long computation...".prependThreadName)
      Thread.sleep(10000)
      42
    }

  // blocking code cannot (usually) be interrupted
  val tryInterrupting: Task[Int] =
    for
      blockingFib <- aBlockingInterruptibleZIO.fork
      _           <- ZIO.succeed("interrupting...").debugThread.delay(1.second) *> blockingFib.interrupt
      mol         <- blockingFib.join
    yield mol

  // set a flag/switch
  def interruptibleBlockingEffect(cancelledFlat: AtomicBoolean): Task[Unit] =
    ZIO.attemptBlockingCancelable {
      (1 to 100000).foreach { element =>
        if !cancelledFlat.get() then
          println(element)
          Thread.sleep(100)
      }
    }(ZIO.succeed(cancelledFlat.set(true)))

  def interruptibleBlockingDemo: Task[Unit] =
    for
      fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
      _   <- ZIO.succeed("interrupting...").debugThread.delay(5.seconds) *> fib.interrupt
      _   <- fib.join
    yield ()

  val chainedZIO: Task[Int] =
    (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> _.debugThread)

  val yieldingDemo: Task[Int]  =
    (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> ZIO.yieldNow *> _.debugThread)

  override def run: ZIO[Any, Any, Any] = yieldingDemo
