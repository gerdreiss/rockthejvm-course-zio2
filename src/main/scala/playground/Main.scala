package playground

import zio.*

object Main extends ZIOAppDefault:

  override def run: Task[Unit] =
    Console.printLine("Hello, ZIO 2!")
