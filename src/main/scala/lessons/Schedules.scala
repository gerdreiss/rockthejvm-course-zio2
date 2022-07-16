package lessons

import utils.*
import zio.*

object Schedules extends ZIOAppDefault:

  val flakyEffect: ZIO[Any, String, String] =
    ZIO
      .ifZIO(Random.nextIntBetween(1, 12).map(_ == 10))(
        ZIO.succeed("fetched value").debugThread,
        ZIO.succeed("Failure").debugThread *> ZIO.fail("error")
      )

  val retried: ZIO[Any, String, String] =
    flakyEffect.retry(Schedule.recurs(9))

  // repeat exactly once
  val oneTimeSchedule             = Schedule.once
  // repeat 9 times
  val recurrentSchedule           = Schedule.recurs(9)
  // repeat every 1 second until a success is return
  val fixedIntervalSchedule       = Schedule.spaced(1.second)
  // repeat every 1 second until a success is return
  val exponentiallySpacedSchedule = Schedule.exponential(100.millis, 2.0)
  // fibonacci schedule
  val fibonacciSchedule           = Schedule.fibonacci(100.millis)
  // sequencing schedules
  val recurrentThanSpacedSchedule = Schedule.recurs(3) ++ Schedule.spaced(1.second)

  // schedules have R = env, I = input, O = output
  val totalElapse =
    Schedule.spaced(1.second) >>>
      Schedule.elapsed
        .map { time =>
          println(s"Elapsed time: $time")
        }

  override def run: ZIO[Any, Any, Any] = flakyEffect.retry(totalElapse)
