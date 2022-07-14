package utils

import zio.ZIO

extension [R, E, A](zio: ZIO[R, E, A])
  def debugThread: ZIO[R, E, A] =
    zio
      .tap(a => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $a")))
      .tapErrorCause(e => ZIO.succeed(println(s"[${Thread.currentThread().getName}][FAIL] $e")))

extension (text: String)
  def lines: List[String] = text.split("\n").toList
  def words: List[String] = text.split("\\s+").toList
