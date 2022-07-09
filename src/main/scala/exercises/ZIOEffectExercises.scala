package exercises

import zio.*

object ZIOEffectExercises:

  def seqTaskLast[R, E, A, B](taskA: ZIO[R, E, A], taskB: ZIO[R, E, B]): ZIO[R, E, B] =
    // for {
    //   a <- taskA
    //   b <- taskB
    // } yield b
    taskA *> taskB

  def seqTaskFirst[R, E, A, B](taskA: ZIO[R, E, A], taskB: ZIO[R, E, B]): ZIO[R, E, A] =
    // for {
    //   a <- taskA
    //   b <- taskB
    // } yield a
    taskA <* taskB

  def runForever[R, E, A](task: ZIO[R, E, A]): ZIO[R, E, A] =
    task.forever

  val endlessLoop = runForever {
    Console.printLine("running...") *> ZIO.sleep(1.second)
  }

  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.as(value)

  def discard[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.unit

  def sum(n: Int): Int =
    if n == 0 then 0 else n + sum(n - 1) // this will crash with StackOverflowError

  def sumZIO(n: Int): UIO[BigInt] =
    if n == 0 then ZIO.succeed(0)
    else
      //   ZIO.suspendSucceed {
      //     ZIO.withParallelism(java.lang.Runtime.getRuntime().availableProcessors()) {
      //       sumZIO(n - 1).map(_ + n)
      //     }
      //   }
      ZIO.suspendSucceed {
        for
          current  <- ZIO.succeed(n)
          previous <- sumZIO(n - 1)
        yield current + previous
      }

  def fibZIO(n: Int): UIO[BigInt] =
    if n <= 2 then ZIO.succeed(1)
    else
      ZIO.suspendSucceed {
        for
          a <- fibZIO(n - 1)
          b <- fibZIO(n - 2)
        yield a + b
      }

  def main(args: Array[String]): Unit =
    Unsafe.unsafe {
      Runtime.default.unsafe.run(fibZIO(100).debug("fibZIO"))
    }
