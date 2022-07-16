package lessons

import utils.*
import zio.*

import java.util.concurrent.{ Executors, ExecutorService }

object AsynchronousEffects extends ZIOAppDefault:

  // CALLBACK-based
  object LoginService:
    case class AuthError(message: String)
    case class UserProfile(name: String, email: String)

    // thread pool
    val executor: ExecutorService =
      Executors.newFixedThreadPool(8)

    // database
    val pwd: Map[String, String] =
      Map("dan@mail.com" -> "123456")

    // the profile data
    val database: Map[String, String] =
      Map("dan@mail.com" -> "Daniel")

    def login(email: String, password: String)(
        onSuccess: UserProfile => Unit,
        onFailure: AuthError => Unit
    ): Unit =
      executor.execute { () =>
        println(s"login($email, $password)".prependThreadName)
        pwd.get(email) match
          case Some(`password`) => onSuccess(UserProfile(database(email), email))
          case Some(_)          => onFailure(AuthError("Incorrect password"))
          case _                => onFailure(AuthError("User not found"))
      }

    def loginZIO(email: String, password: String): ZIO[Any, AuthError, UserProfile] =
      ZIO.async { cb => // callback object
        login(email, password)(profile => cb(ZIO.succeed(profile)), error => cb(ZIO.fail(error)))
      }

  val loginProgram: ZIO[Any, Object, Unit] =
    for
      email    <- Console.readLine("Email: ")
      password <- Console.readLine("Password: ")
      profile  <- LoginService.loginZIO(email, password).debugThread
      _        <- Console.printLine(s"Welcome, ${profile.name}")
    yield ()

  override def run: ZIO[Any, Any, Any] =
    loginProgram
