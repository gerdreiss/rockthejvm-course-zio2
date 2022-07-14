package lessons

import utils.*
import zio.*

import java.io.IOException
import scala.io.StdIn
import scala.util.{ Either, Try }

object ZIOErrorHandling extends ZIOAppDefault:

  def enterNum: Int =
    StdIn.readLine("Enter a number: ").toInt

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

  /**
   * Errors = failures present in the ZIO type signature
   * Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature
   * 
   * ZIO[R, E, A] can finish with Exit[E, A]
   *   - Success[A] containing a value
   *   - Cause[E]
   *     - Fail[E] containing an error
   *     - Die(t: Throwable) which was unforeseen
   */

  val failedInt: ZIO[Any, String, Int]                  = ZIO.fail("This is a failure")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int]         = failureCauseExposed.unsandbox

  val foldedWithCause: URIO[Any, String] =
    failedInt
      .foldCause(
        {
          case Cause.Fail(error, trace) => s"Recoverable error: $error\nTrace: $trace"
          case Cause.Die(death, trace)  => s"The Doom: $death\nTrace: $trace"
          case other                    => s"Unexpected error: $other"
        },
        i => s"You entered $i"
      )

  def callHttpEndpoint(urlString: String): ZIO[Any, Throwable, String] =
    ZIO.scoped {
      for
        url     <- ZIO.attempt(new java.net.URL(urlString))
        content <- ZIO
                     .fromAutoCloseable(ZIO.attempt(url.openStream()))
                     .map(_.readAllBytes())
                     .map(new String(_))
      yield content
    }

  case class IndexError(message: Int)
  case class DbError(message: String)

  val callApi: ZIO[Any, IndexError, String] = ZIO.succeed("result 1")
  val dbQuery: ZIO[Any, DbError, String]    = ZIO.succeed("result 2")

  val combined: ZIO[Any, IndexError | DbError, String] =
    for
      apiResult <- callApi
      dbResult  <- dbQuery
    yield s"$apiResult and $dbResult"

  override def run: ZIO[Any, IOException, String] =
    callHttpEndpoint("http://localhost:8080").refineOrDie {
      case ioe: IOException => ioe
      case e                => new IOException(e)

    }.debug
