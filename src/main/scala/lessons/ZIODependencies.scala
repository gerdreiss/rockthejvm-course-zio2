package lessons

import zio.*

object ZIODependencies extends ZIOAppDefault:

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  case class Connection():
    def runQuery(query: String): Task[Unit] =
      Console.printLine(s"Running query: $query")

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase):
    def subscribeUser(user: User): Task[Unit] =
      emailService.sendEmail(user) *>
        userDatabase.insertUser(user)

  object UserSubscription:
    def make(emailService: EmailService, userDatabase: UserDatabase): UserSubscription =
      new UserSubscription(emailService, userDatabase)

  class EmailService():
    def sendEmail(user: User): Task[Unit] =
      Console.printLine(s"Subscribed user ${user.name} to newsletter")

  object EmailService:
    def make: EmailService =
      new EmailService()

  class UserDatabase(connectionPool: ConnectionPool):
    def insertUser(user: User): Task[Unit] =
      connectionPool.getConnection
        .flatMap {
          _.runQuery(s"INSERT INTO users (name, email) VALUES ('${user.name}', '${user.email}')")
        }

  object UserDatabase:
    def make(connectionPool: ConnectionPool): UserDatabase =
      new UserDatabase(connectionPool)

  class ConnectionPool(numConns: Int):
    def getConnection: Task[Connection] =
      Console
        .printLine(s"Connected to database.")
        .as(Connection())

  object ConnectionPool:
    def make(numConns: Int): ConnectionPool =
      new ConnectionPool(numConns)

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
    for
      subscription <- ZIO.service[UserSubscription]
      _            <- subscription.subscribeUser(user)
    yield ()

  override def run: ZIO[Any, Any, Any] =
    subscribe2(User("John", "john@mail.com"))
      .provideLayer(
        ZLayer.succeed(
          UserSubscription.make(
            EmailService.make,
            UserDatabase.make(
              ConnectionPool.make(10)
            )
          )
        )
      )
