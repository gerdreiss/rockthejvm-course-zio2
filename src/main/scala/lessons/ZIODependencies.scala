package lessons

import utils.*
import zio.*

import java.io.IOException
import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault:

  val subscriptionService: UIO[UserSubscription] =
    ZIO.succeed(
      UserSubscription.make(
        EmailService.make,
        UserDatabase.make(
          ConnectionPool.make(10)
        )
      )
    )

  def subscribe(user: User): ZIO[Any, Throwable, Unit] =
    subscriptionService.flatMap(_.subscribeUser(user))

  // alternative
  def subscribe2(user: User): ZIO[UserSubscription, Throwable, Unit] =
    // for
    //   subscription <- ZIO.service[UserSubscription]
    //   _            <- subscription.subscribeUser(user)
    // yield ()
    ZIO.serviceWithZIO[UserSubscription](_.subscribeUser(user))

  /**
   * Layers
   */
  val connectionPoolLayer: ULayer[ConnectionPool] =
    ZLayer.succeed(ConnectionPool.make(10))

  val databaseLayer: URLayer[ConnectionPool, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.make)

  val databaseLayerFull: ULayer[UserDatabase] =
    connectionPoolLayer >>> databaseLayer

  val emailServiceLayer: ULayer[EmailService] =
    ZLayer.succeed(EmailService.make)

  val subscriptionRequirementLayer: ULayer[UserDatabase & EmailService] =
    databaseLayerFull ++ emailServiceLayer

  val userSubscriptionLayer: URLayer[EmailService & UserDatabase, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.make)

  val userSubscriptionLayerFull: ULayer[UserSubscription] =
    subscriptionRequirementLayer >>> userSubscriptionLayer

  val userSubscriptionLayerFull2: ULayer[UserSubscription] =
    ZLayer.make[UserSubscription](
      ConnectionPool.live(10),
      UserDatabase.live.fresh, // new instance for each layer
      EmailService.live,
      UserSubscription.live
    )

  val dbWithPoolLayer: URLayer[ConnectionPool, ConnectionPool & UserDatabase] =
    UserDatabase.live.passthrough

  val dbService: URLayer[UserDatabase, UserDatabase] =
    ZLayer.service[UserDatabase]

  val subscriptionLaunch: URIO[EmailService & UserDatabase, Nothing] =
    UserSubscription.live.launch

  /**
   * Standard services:
   *  - Clock
   *  - Random
   *  - System
   *  - Console
   */
  val getTime: UIO[Long]                                = Clock.currentTime(TimeUnit.SECONDS)
  val randomValue: UIO[Int]                             = Random.nextInt
  val systemInfo: IO[SecurityException, Option[String]] = System.env("os.name")
  val printEffect: IO[IOException, Unit]                = Console.printLine("This is ZIO")

  override def run: Task[Unit] =
    subscribe2(User("John", "john@mail.com"))
      .provide(userSubscriptionLayerFull2)
