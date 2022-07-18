package lessons

import utils.*
import zio.*

object Promises extends ZIOAppDefault:

  val promise: UIO[Promise[Throwable, Int]] =
    Promise.make[Throwable, Int]

  // await - block the fiber until the promise has a value
  val reader: Task[Int] =
    promise.flatMap(_.await)

  // succeed, fail, complete
  val writer: UIO[Boolean] =
    promise.flatMap(_.succeed(42))

  def demoPromise(): Task[Unit] =
    // producer - consumer problem
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] =
      for
        _   <- ZIO.succeed("[consumer] waiting for result...").debugThread
        mol <- promise.await
        _   <- ZIO.succeed("[consumer] result: " + mol).debugThread
      yield ()

    def producer(promise: Promise[Throwable, Int]): Task[Unit] =
      for
        _   <- ZIO.succeed("[producer] crunching numbers...").debugThread
        mol <- ZIO.succeed(42).delay(3.seconds)
        _   <- ZIO.succeed("[producer] complete.").debugThread
        _   <- promise.succeed(mol)
      yield ()

    for
      promise <- Promise.make[Throwable, Int]
      _       <- consumer(promise) <&> producer(promise)
    yield ()

  end demoPromise

  val fileParts: List[String] =
    List("I ", "love S", "cala", " with pure FP an", "d ZIO<EOF>")

  def downloadFileWithRef(): UIO[Unit] =
    def downloadFile(content: Ref[String]): UIO[Unit] =
      ZIO.collectAllDiscard {
        fileParts.map { part =>
          content.update(_ + part) *>
            ZIO.succeed(s"got '$part'").delay(1.second).debugThread
        }
      }

    def printFile(file: String) =
      ZIO.succeed("File download complete: ").debugThread *>
        ZIO.succeed(file).debugThread

    def continueDownload(content: Ref[String]) =
      ZIO.succeed("downloading...").debugThread *>
        notifyFileComplete(content).delay(500.millis)

    def notifyFileComplete(content: Ref[String]): UIO[Unit] =
      for
        file <- content.get
        _    <- if file.endsWith("<EOF>") then printFile(file)
                else continueDownload(content)
      yield ()

    for
      content <- Ref.make("")
      _       <- downloadFile(content) <&> notifyFileComplete(content)
    yield ()

  end downloadFileWithRef

  def downloadFileWithRefAndPromise(): Task[Unit] =
    def downloadFile(content: Ref[String], promise: Promise[Throwable, String]): UIO[Unit] =
      ZIO.collectAllDiscard {
        fileParts.map { part =>
          for
            _    <- ZIO.succeed(s"got '$part'").delay(1.second).debugThread
            file <- content.updateAndGet(_ + part)
            _    <- promise
                      .succeed(file.replace("<EOF>", ""))
                      .when(file.endsWith("<EOF>"))
          yield ()
        }
      }

    def notifyFileComplete(promise: Promise[Throwable, String]): Task[Unit] =
      for
        _    <- ZIO.succeed("downloading...").debugThread
        file <- promise.await
        _    <- ZIO.succeed(s"File download complete: $file").debugThread
      yield ()

    for
      content <- Ref.make("")
      promise <- Promise.make[Throwable, String]
      _       <- downloadFile(content, promise) <&> notifyFileComplete(promise)
    yield ()

  end downloadFileWithRefAndPromise

  override def run: ZIO[Any, Any, Any] =
    downloadFileWithRefAndPromise()
