package lessons

import zio.*
import scala.io.StdIn
import scala.util.Try
import scala.util.Either

object ZIOErrorHandling extends ZIOAppDefault:

  def enterNum = StdIn.readLine("Enter a number: ").toInt

  val thisCanFail: ZIO[Any, Throwable, Int] =
    ZIO
      .attempt(enterNum)
      .catchSome { case e: NumberFormatException =>
        Console.printLine("You didn't enter a number!") *> thisCanFail
      }

  val folded: UIO[String] =
    thisCanFail
      .fold(
        e => s"Error: $e",
        i => s"You entered $i"
      )

  val foldedZIO: UIO[String] =
    thisCanFail
      .foldZIO(
        e => ZIO.succeed(s"Error: $e"),
        i => ZIO.succeed(s"You entered $i")
      )

  val tried: Task[Int]                   = ZIO.fromTry(Try(enterNum))
  val eithered: IO[Throwable, Int]       = ZIO.fromEither(Try(enterNum).toEither)
  val optioned: IO[Option[Nothing], Int] = ZIO.fromOption(Try(enterNum).toOption)

  override def run = thisCanFail.map(_ * 2).debug
