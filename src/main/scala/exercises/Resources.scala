package exercises

import utils.*
import zio.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault:

  def openFileScanner(path: String): Task[Scanner] =
    ZIO.attempt(new Scanner(new File(path)))

  def acquireOpenFileG(path: String): Task[Unit] =
    Console.printLine(s"Opening file $path...") *>
      ZIO.acquireReleaseWith(openFileScanner(path))(scanner =>
        ZIO.succeed(scanner.close()) <* Console.printLine("File closed...").ignore
      ) { scanner =>
        Console
          .printLine(scanner.nextLine)
          .delay(100.millis)
          .repeatWhile(_ => scanner.hasNextLine)
      }

  def acquireOpenFileD(path: String): Task[Unit] =
    ZIO.succeed(s"Opening file $path...").debugThread *>
      ZIO.acquireReleaseWith(
        openFileScanner(path) // acquire
      ) { scanner =>
        ZIO.succeed(s"closing file at $path").debugThread *>
          ZIO.succeed(scanner.close()) // close
      } { scanner =>
        def readLines: UIO[Unit] =
          if scanner.hasNextLine then
            ZIO.succeed(scanner.nextLine()).debugThread *>
              ZIO.sleep(100.millis) *>
              readLines
          else ZIO.unit

        readLines
      }

  val testAcquireOpenFileG: Task[Unit] =
    for
      fib <- acquireOpenFileG("src/main/resources/text_01.txt")
               .onInterrupt(Console.printLine("reading file interrupted...").ignore)
               .fork
      _   <- fib.interrupt.delay(2.seconds)
    yield ()

  val testAcquireOpenFileD: Task[Unit] =
    for
      fib <- acquireOpenFileG("src/main/resources/text_01.txt").fork
      _   <- fib.interrupt.delay(2.seconds)
    yield ()

  override def run = testAcquireOpenFileD
