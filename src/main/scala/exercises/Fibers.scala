package exercises

import zio.*

import java.nio.file.Files
import java.nio.file.Paths
import java.util.stream.Collectors.toList
import scala.jdk.CollectionConverters.*

object Fibers extends ZIOAppDefault:

  // part 1 - an effect which reads one file and counts words
  def words(path: String): Task[Int] =
    ZIO.acquireReleaseWith(ZIO.attempt(io.Source.fromFile(path))) { source =>
      ZIO.attempt(source.close()).ignore
    } { source =>
      ZIO.attemptBlockingIO(source.mkString.split("\\s+").length)
    }

  // part 2 - spin up fibers for all the files
  def parWords: Task[Int] =
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
        Files
          .list(Paths.get("src/main/resources/"))
          .map(_.toString())
          .collect(toList())
          .asScala
      )(words)
      .map(_.sum)

  override def run = parWords.debug
