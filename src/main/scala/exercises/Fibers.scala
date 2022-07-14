package exercises

import utils.*
import zio.*
import zio.nio.file.{Files as ZFiles, Path as ZPath}
import zio.stream.{Stream, ZStream}

import java.nio.file.{Files as JFiles, Paths as JPaths}
import java.util.stream.Collectors.toList
import scala.jdk.CollectionConverters.*

object Fibers extends ZIOAppDefault:

  private val RESOURCES: String   = "src/main/resources/"
  private val NUM_PROCESSORS: Int = java.lang.Runtime.getRuntime.availableProcessors()

  def readLines(path: ZPath): Stream[Throwable, String] =
    ZStream
      .acquireReleaseWith(ZIO.attempt(io.Source.fromFile(path.toFile))) { source =>
        ZIO.attempt(source.close()).ignore
      }
      .flatMap { source =>
        ZStream.fromIterator(source.getLines())
      }

  // part 1 - an effect which reads one file and counts words
  def countWords(path: String): Task[Int] =
    ZIO
      .acquireReleaseWith(ZIO.attempt(io.Source.fromFile(path))) { source =>
        ZIO.attempt(source.close()).ignore
      } { source =>
        ZIO.attempt(source.mkString.words.length)
      }

  val paths: List[String] = JFiles
    .list(JPaths.get(RESOURCES))
    .map(_.toString)
    .collect(toList)
    .asScala
    .toList

  // part 2 - spin up fibers for all the files
  // Daniel's solution using fibers
  val parWordsD: Task[Int] =
    paths
      .map(countWords)
      .map(_.fork)
      .map(_.flatMap(_.join))
      .reduce { (a, b) =>
        a.zipWith(b)(_ + _)
      }
  // my solution using ZIO's built in parallelism
  val parWordsG: Task[Int] =
    ZIO
      .foreachPar(paths)(countWords)
      .map(_.sum)

  // exercises for Parallelism
  val collected: Task[Int] = ZIO.collectAllPar(paths.map(countWords)).map(_.sum)
  val reduced: Task[Int]   = ZIO.reduceAllPar(ZIO.succeed(0), paths.map(countWords))(_ + _)
  val merged: Task[Int]    = ZIO.mergeAllPar(paths.map(countWords))(0)(_ + _)

  // Experimenting with zio-nio
  val wordsFromStream: Task[Int] =
    ZFiles
      .list(ZPath(RESOURCES))
      .flatMapPar(NUM_PROCESSORS)(readLines)
      .map(_.words.length)
      .runFold(0)(_ + _)

  override def run: ZIO[Any, Throwable, Int] =
    parWordsG.debug *>
      collected.debug *>
      reduced.debug *>
      merged.debug *>
      wordsFromStream.debug
