package exercises

import zio.*
import zio.nio.file.{ Files as ZFiles, Path as ZPath }
import zio.stream.ZStream

import java.lang.Runtime as JRuntime
import java.nio.file.{ Files as JFiles, Paths as JPaths }
import java.util.stream.Collectors.toList
import scala.jdk.CollectionConverters.*

object Fibers extends ZIOAppDefault:

  private val RESOURCES: String   = "src/main/resources/"
  private val NUM_PROCESSORS: Int = JRuntime.getRuntime.availableProcessors()

  // part 1 - an effect which reads one file and counts words
  def words(path: String): Task[Int] =
    ZIO.acquireReleaseWith(ZIO.attempt(io.Source.fromFile(path))) { source =>
      ZIO.attempt(source.close()).ignore
    } { source =>
      ZIO.attemptBlockingIO(source.mkString.split("\\s+").length)
    }

  // part 2 - spin up fibers for all the files
  val parWords: Task[Int] =
    // Daniel's solution using fibers
    // Files
    //   .list(Paths.get("src/main/resources/"))
    //   .collect(toList())
    //   .asScala
    //   .map(path => words(path.toString))
    //   .map(_.fork)
    //   .map(_.flatMap(_.join))
    //   .reduce { (a, b) =>
    //     a.zipWith(b)(_ + _)
    //   }
    // my solution using ZIO's built in parallelism
    ZIO
      .foreachPar(
        JFiles
          .list(JPaths.get(RESOURCES))
          .map(_.toString())
          .collect(toList)
          .asScala
      )(words)
      .map(_.sum)

  val wordsFromStream: Task[Int] =
    ZFiles
      .list(ZPath(RESOURCES))
      .flatMapPar(NUM_PROCESSORS) { path =>
        ZStream
          .acquireReleaseWith(ZIO.attempt(io.Source.fromFile(path.toFile))) { source =>
            ZIO.attempt(source.close()).ignore
          }
          .flatMap { source =>
            ZStream.fromIterator(source.getLines())
          }
      }
      .map(_.split("\\W+").length)
      .runFold(0)(_ + _)

  override def run: ZIO[Any, Throwable, Int] =
    parWords.debug *> wordsFromStream.debug
