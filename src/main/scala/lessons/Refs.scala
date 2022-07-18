package lessons

import utils.*
import zio.*
import zio.stream.*

import scala.io.BufferedSource

object Refs extends ZIOAppDefault:

  val loremIpsumUrl = "https://loripsum.net/api/plaintext/25"

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

  // val text: List[String] =
  //  List(
  //    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
  //    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
  //    "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
  //    "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  //  )

  val loremIpsumText: Task[List[String]] =
    ZIO.acquireReleaseWith {
      ZIO.attempt(io.Source.fromURL(loremIpsumUrl))
    } { source =>
      ZIO.attempt(source.close()).ignore
    } { source =>
      ZIO.attemptBlocking(source.getLines().toList)
    }

  val loremIpsumStream: ZStream[Any, Throwable, String] =
    ZStream.acquireReleaseWith {
      ZIO.attempt(io.Source.fromURL(loremIpsumUrl))
    } { source =>
      ZIO.attempt(source.close()).ignore
    } flatMap { source =>
      zio.stream.ZStream.fromIterator(source.getLines())
    }

  val totalCount: Task[(Ref[Int], List[Int])] =
    for
      text    <- loremIpsumText
      total   <- Ref.make(0)
      updates <- ZIO.foreachPar(text)(countWords(_, total))
    yield (total, updates.sorted)

  val totalCountStream: Task[Int] =
    loremIpsumStream.map(_.words.size).runSum

  override def run: ZIO[Any, Any, Any] =
    totalCount.debugThread("Total count: ") *>
      totalCountStream.debugThread("Total count from stream: ")
