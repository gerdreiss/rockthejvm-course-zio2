package lessons

import utils.*
import zio.*

// app to subscribe users to newsletter
case class User(name: String, email: String)

case class Connection():
  def runQuery(query: String): Task[Unit] =
    Console.printLine(s"Running query: $query")

class EmailService():
  def sendEmail(user: User): Task[Unit] =
    Console.printLine(s"Subscribed user ${user.name} to newsletter")

object EmailService:
  def make: EmailService =
    new EmailService()

  val live: ULayer[EmailService] =
    ZLayer.succeed(make)

class ConnectionPool(numConns: Int):
  def getConnection: Task[Connection] =
    Console
      .printLine(s"Connected to database.")
      .as(Connection())

object ConnectionPool:
  def make(numConns: Int): ConnectionPool =
    new ConnectionPool(numConns)

  def live(numConns: Int): ULayer[ConnectionPool] =
    ZLayer.succeed(make(numConns))

class UserDatabase(connectionPool: ConnectionPool):
  def insertUser(user: User): Task[Unit] =
    connectionPool.getConnection
      .flatMap {
        _.runQuery(s"INSERT INTO users (name, email) VALUES ('${user.name}', '${user.email}')")
      }

object UserDatabase:
  def make(connectionPool: ConnectionPool): UserDatabase =
    new UserDatabase(connectionPool)

  val live: URLayer[ConnectionPool, UserDatabase] =
    ZLayer.fromFunction(make)

class UserSubscription(emailService: EmailService, userDatabase: UserDatabase):
  def subscribeUser(user: User): Task[Unit] =
    emailService.sendEmail(user) *>
      userDatabase.insertUser(user)

object UserSubscription:
  def make(emailService: EmailService, userDatabase: UserDatabase): UserSubscription =
    new UserSubscription(emailService, userDatabase)

  val live: URLayer[EmailService & UserDatabase, UserSubscription] =
    ZLayer.fromFunction(make)
