package exercises

import utils.*
import zio.*

import scala.collection.immutable.Queue as ScalaQueue

abstract class Mutex:
  def acquire: UIO[Unit]
  def release: UIO[Unit]

object Mutex:

  type Signal = Promise[Nothing, Unit]

  case class State(locked: Boolean, waiting: ScalaQueue[Signal])
  object State:
    val unlocked: State = State(false, ScalaQueue.empty)

  def createSimpleMutex(state: Ref[State]): Mutex =
    new Mutex:
      def acquire: UIO[Unit] =
        Promise.make[Nothing, Unit]
          .flatMap { signal =>
            state.modify {
              case State(false, _)      => ZIO.unit     -> State(true, ScalaQueue.empty)
              case State(true, waiting) => signal.await -> State(true, waiting.enqueue(signal))
            }
          }
          .flatten

      def release: UIO[Unit] =
        state
          .modify {
            case State(false, _)      => ZIO.unit -> State.unlocked
            case State(true, waiting) =>
              if waiting.isEmpty then ZIO.unit -> State.unlocked
              else
                val (sig, rest) = waiting.dequeue
                sig.succeed(()).unit -> State(true, rest)
          }
          .flatten
  end createSimpleMutex

  def createInterruptibleMutex(state: Ref[State]): Mutex =
    new Mutex:
      def acquire: UIO[Unit] =
        ZIO.uninterruptibleMask { restore =>
          Promise.make[Nothing, Unit]
            .flatMap { signal =>

              val cleanup: UIO[Unit] =
                state
                  .modify {
                    case State(flag, waiting) =>
                      val newWaiting = waiting.filterNot(_ eq signal)
                      val decision   = if newWaiting != waiting then ZIO.unit else release
                      decision -> State(flag, newWaiting)
                  }
                  .flatten

              state.modify {
                case State(false, _)      =>
                  ZIO.unit -> State(true, ScalaQueue.empty)
                case State(true, waiting) =>
                  restore(signal.await).onInterrupt(cleanup) -> State(true, waiting.enqueue(signal))
              }
            }
            .flatten
        }

      def release: UIO[Unit] =
        state
          .modify {
            case State(false, _)      => ZIO.unit -> State.unlocked
            case State(true, waiting) =>
              if waiting.isEmpty then ZIO.unit -> State.unlocked
              else
                val (sig, rest) = waiting.dequeue
                sig.succeed(()).unit -> State(true, rest)
          }
          .flatten
  end createInterruptibleMutex

  def make: UIO[Mutex] =
    Ref.make(State.unlocked).map(createInterruptibleMutex)

object MutexPlayground extends ZIOAppDefault:

  def workInCriticalRegion(): UIO[Int] =
    Random.nextIntBounded(100).delay(1.second)

  def demoNonLockingTasks: UIO[Unit] =
    ZIO.collectAllParDiscard((1 to 10).toList
      .map { i =>
        for
          _      <- ZIO.succeed(s"[task $i] working...").debugThread
          result <- workInCriticalRegion()
          _      <- ZIO.succeed(s"[task $i] got result: $result").debugThread
        yield ()
      })

  def createTask(id: Int, mutex: Mutex): UIO[Int] =
    val task =
      for
        _      <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
        _      <- mutex.acquire
        // critical region
        _      <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
        result <- workInCriticalRegion().onInterrupt(mutex.release)
        _      <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
        // end critical region
        _      <- mutex.release
      yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] interrupted").debugThread)
      .onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause").debugThread)

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if id % 2 == 0 then createTask(id, mutex)
    else
      for
        fib    <- createTask(id, mutex).fork
        _      <- ZIO.succeed(s"interrupting task $id").debugThread.delay(2500.millis) *> fib.interrupt
        result <- fib.join
      yield result

  def demoLockingTasks: UIO[Unit] =
    for
      mutex <- Mutex.make
      _     <- ZIO.collectAllParDiscard((1 to 10)
                 .map { i =>
                   createTask(i, mutex)
                 })
    yield ()

  def demoInterruptingTasks: UIO[Unit] =
    for
      mutex <- Mutex.make

      fib_01 <- createInterruptingTask(1, mutex).fork
      fib_02 <- createInterruptingTask(2, mutex).fork
      fib_03 <- createInterruptingTask(3, mutex).fork
      fib_04 <- createInterruptingTask(4, mutex).fork
      fib_05 <- createInterruptingTask(5, mutex).fork
      fib_06 <- createInterruptingTask(6, mutex).fork
      fib_07 <- createInterruptingTask(7, mutex).fork
      fib_08 <- createInterruptingTask(8, mutex).fork
      fib_09 <- createInterruptingTask(9, mutex).fork
      fib_10 <- createInterruptingTask(10, mutex).fork

      _ <- fib_01.await
      _ <- fib_02.await
      _ <- fib_03.await
      _ <- fib_04.await
      _ <- fib_05.await
      _ <- fib_06.await
      _ <- fib_07.await
      _ <- fib_08.await
      _ <- fib_09.await
      _ <- fib_10.await
    yield ()

  override def run: ZIO[Any, Any, Any] = demoInterruptingTasks
