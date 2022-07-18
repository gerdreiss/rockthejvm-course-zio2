package lessons

import utils.*
import zio.*

object Refs extends ZIOAppDefault:

  val atomicMOL: UIO[Ref[Int]] = Ref.make(42)

  val mol: UIO[Int]       = atomicMOL.flatMap(_.get)           // thread-safe getter
  val updated: UIO[Unit]  = atomicMOL.flatMap(_.set(1))        // thread-safe setter
  val updated2: UIO[Int]  = atomicMOL.flatMap(_.getAndSet(1))  // thread-safe getter and setter
  val updated3: UIO[Unit] = atomicMOL.flatMap(_.update(_ + 1)) // thread-safe updater

  val updatedModWithValue: UIO[Int] =
    // atomicMOL.flatMap(_.getAndUpdate(_ + 1)) // thread-safe updater and getter
    atomicMOL.flatMap(_.updateAndGet(_ + 1)) // thread-safe updater and getter

  val modified: UIO[String] =
    atomicMOL.flatMap(_.modify { v => // thread-safe updater and getter
      (s"this is the current value: $v", v + 1)
    })

  def countWords(s: String, total: Ref[Int]): UIO[Int] =
    total.updateAndGet(_ + s.words.size).debugThread("New total: ")

  val text: List[String] =
    List(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
      "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
      "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    )

  val totalCount: UIO[Ref[Int]] =
    for
      total <- Ref.make(0)
      _     <- ZIO.foreachParDiscard(text)(countWords(_, total))
    yield total

  override def run: ZIO[Any, Any, Any] = totalCount.debugThread("Total count: ")
