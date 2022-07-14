package lessons

import utils.*
import zio.*

import java.io.File
import java.util.Scanner

/**
 * Managing resource lifecycle
 */
object Resources extends ZIOAppDefault:

  def unsafeMethod: Int    = throw new RuntimeException("error")
  def anAttempt: Task[Int] = ZIO.attempt(unsafeMethod)

  // finalizers
  val attemptWithFinalizer: Task[Int] =
    anAttempt.ensuring(ZIO.succeed("finalized!").debugThread)

  // multiple finalizers
  val attemptWithMultipleFinalizers: Task[Int] =
    anAttempt
      .ensuring(ZIO.succeed("finalized!").debugThread)
      .ensuring(ZIO.succeed("once more finalized!").debugThread)

  // .onInterrupt, .onError, .onDone, .onExit
  class Connection private (url: String):
    def open(): UIO[String]  = ZIO.succeed("open").debugThread
    def close(): UIO[String] = ZIO.succeed("closed").debugThread

  object Connection:
    def make(url: String): UIO[Connection] =
      ZIO.succeed(new Connection(url))

  val leakingResources: UIO[Unit] =
    for
      conn <- Connection.make("rockthejvm.com")
      fib  <- (conn.open() *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.succeed("interrupting").debugThread.delay(1.second) *> fib.interrupt
      _    <- fib.join
    yield ()

  val correctHandlingResourcesEnsuring: UIO[Unit] =
    for
      conn <- Connection.make("rockthejvm.com")
      fib  <- (conn.open() *> ZIO.sleep(300.seconds)).ensuring(conn.close()).fork // <<- tedious
      _    <- ZIO.succeed("interrupting").debugThread.delay(1.second) *> fib.interrupt
      _    <- fib.join
    yield ()

  // acquire, release, and finalizers
  val connectionResource: URIO[Scope, Connection] =
    ZIO.acquireRelease(Connection.make("rockthejvm.com"))(_.close())

  val usingScopedResource: URIO[Scope, Unit] =
    for
      conn <- connectionResource
      fib  <- (conn.open() *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.succeed("interrupting...").debugThread.delay(1.second) *> fib.interrupt
      _    <- fib.join
    yield ()

  val scopedResource: UIO[Unit] = ZIO.scoped(usingScopedResource)

  val connectionResource2: UIO[Unit] =
    ZIO.acquireReleaseWith(Connection.make("rockthejvm.com"))(_.close()) { conn =>
      conn.open() *> ZIO.sleep(300.seconds)
    }

  val usingConnectionResource2: UIO[Unit] =
    for
      fib <- connectionResource2.fork
      _   <- ZIO.succeed("interrupting...").debugThread.delay(1.second) *> fib.interrupt
      _   <- fib.join
    yield ()

  // nested resources
  def connectionFromConfig(path: String): Task[Unit] =
    ZIO.acquireReleaseWith(ZIO.attempt(new Scanner(new File(path)))) { scanner =>
      ZIO.succeed(s"closing file at $path").debugThread *>
        ZIO.succeed(scanner.close()) // close
    } { scanner =>
      ZIO.acquireReleaseWith(Connection.make(scanner.nextLine))(_.close()) { conn =>
        conn.open() *> ZIO.never
      }
    }

  def connectionFromConfig2(path: String): ZIO[Scope, Throwable, Unit] =
    for
      scanner <- ZIO.acquireRelease(ZIO.attempt(new Scanner(new File(path)))) { scanner =>
                   ZIO.succeed(s"closing file at $path").debugThread *>
                     ZIO.succeed(scanner.close()) // close
                 }
      conn    <- ZIO.acquireRelease(Connection.make(scanner.nextLine))(_.close())
      _       <- conn.open() *> ZIO.never
    yield ()

  override def run = connectionFromConfig2("src/main/resources/connection.conf")
