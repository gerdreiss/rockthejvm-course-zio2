package exercises

import zio.*
import scala.util.Try
import scala.util.Success
import scala.util.Failure

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
