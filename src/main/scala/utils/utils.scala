package utils

import zio.ZIO

extension [R, E, A](zio: ZIO[R, E, A])
  def debugThread: ZIO[R, E, A] =
    zio
      .tap(a => ZIO.succeed(println(s"$a".prependThreadName)))
      .tapErrorCause(e => ZIO.succeed(println(s"$e".prependThreadName)))

extension (text: String)
  def lines: List[String]       = text.split("\n").toList
  def words: List[String]       = text.split("\\s+").toList
  def prependThreadName: String = s"[${Thread.currentThread.getName}] $text"
