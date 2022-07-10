package exercises

import zio.*
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.io.IOException

object ZIOErrorHandling:

  def fromTry[A](input: Try[A]): Task[A] =
    // input match
    //   case Success(a) => ZIO.succeed(a)
    //   case Failure(e) => ZIO.fail(e)
    input.fold(ZIO.fail, ZIO.succeed)

  def fromOption[A](input: Option[A]): ZIO[Any, Option[Nothing], A] =
    // input match
    //   case Some(a) => ZIO.succeed(a)
    //   case None    => ZIO.fail(None)
    input.fold(ZIO.fail(None))(ZIO.succeed)

  def fromEither[E, A](input: Either[E, A]): ZIO[Any, E, A] =
    // input match
    //   case Left(e)  => ZIO.fail(e)
    //   case Right(a) => ZIO.succeed(a)
    input.fold(ZIO.fail, ZIO.succeed)

  def either[R, E, A](input: ZIO[R, E, A]): ZIO[R, Nothing, Either[E, A]] =
    // input.map(Right(_)).catchAll(e => ZIO.succeed(Left(e)))
    // input.foldZIO(
    //   e => ZIO.succeed(Left(e)),
    //   a => ZIO.succeed(Right(a))
    // )
    input.fold(Left.apply, Right.apply)

  def absolve[R, E, A](input: ZIO[R, Nothing, Either[E, A]]): ZIO[R, E, A] =
    input.flatMap(_.fold(ZIO.fail, ZIO.succeed))

  //
  val badFailure: ZIO[Any, Nothing, Int] =
    ZIO.succeed[Int](throw new Exception("bad"))

  val betterFailure: ZIO[Any, Cause[Nothing], Int] =
    badFailure.sandbox

  val evenBetterFailure: ZIO[Any, Exception, Int] =
    badFailure.unrefineTo[Exception]

  //
  def narrowError[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie { case e: IOException => e }

  //
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    // Dan's solution
    // zio.foldZIO(
    //   e => ZIO.fail(Left(e)),
    //   {
    //     case Left(e)  => ZIO.fail(Right(e))
    //     case Right(b) => ZIO.succeed(b)
    //   }
    // )
    zio
      .mapError(Left.apply)
      .flatMap {
        case Left(e)  => ZIO.fail(Right(e))
        case Right(b) => ZIO.succeed(b)
      }

  //
  val db = Map(
    "daniel" -> 123,
    "alice"  -> 234,
    "bob"    -> 345
  )
  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if userId == userId.toLowerCase()
    then ZIO.succeed(db.get(userId).map(UserProfile(userId, _)))
    else ZIO.fail(QueryError("Invalid user id"))

  def betterLookupProfileG(userId: String): ZIO[Any, QueryError, UserProfile] =
    ZIO
      .fromOption(db.get(userId))
      .map(UserProfile(userId, _))
      .mapError(_ => QueryError(s"User with ID $userId not found"))

  def betterLookupProfileD(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    // lookupProfile(userId).foldZIO(
    //   e => ZIO.fail(Some(e)),
    //   {
    //     case Some(profile) => ZIO.succeed(profile)
    //     case None          => ZIO.fail(None)
    //   }
    // )
    lookupProfile(userId).some
