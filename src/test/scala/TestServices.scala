import zio.*
import zio.test.Assertion.*
import zio.test.TestAspect.*
import zio.test.*

object SimpleDependencySpec extends ZIOSpecDefault:
  def spec = test("simple dependency") {
    val zio: URIO[Int, Int] = ZIO.serviceWith[Int](_ * 42)
    assertZIO(zio)(equalTo(420))
  }.provide(ZLayer.succeed(10))

object BusinessLogicSpec extends ZIOSpecDefault:

  // dependency
  abstract class Database[K, V]:
    def get(key: K): Task[V]
    def put(key: K, value: V): Task[Unit]

  object Database:
    def make(url: String): UIO[Database[String, String]] = ??? // the REAL thing

  // business logic
  def normalizeUsername(username: String): UIO[String] = ZIO.succeed(username.toUpperCase())

  val mockedDb = ZIO.succeed {
    new Database[String, String]:
      private val db = collection.mutable.Map.empty[String, String]

      def get(key: String): Task[String] = ZIO.attempt(db(key))

      def put(key: String, value: String): Task[Unit] = ZIO.succeed(db.put(key, value))
  }

  def spec = suite("A user survey application should...")(
    test("normalize user names") {
      val surveyPreliminaryLogic =
        for
          db         <- ZIO.service[Database[String, String]]
          _          <- db.put("123", "joe")
          username   <- db.get("123")
          normalized <- normalizeUsername(username)
        yield normalized

      assertZIO(surveyPreliminaryLogic)(equalTo("JOE"))
    }
  ).provideShared(ZLayer.fromZIO(mockedDb))

end BusinessLogicSpec

/**
 * Built-in test services
 * - console
 * - random
 * - clock
 * - system
 */
object DummyConsoleApp:
  def welcomeUser: Task[Unit] =
    for
      name <- Console.readLine("Name: ")
      _    <- Console.printLine(s"Welcome, $name!")
    yield ()

object BuiltInTestServicesSpec extends ZIOSpecDefault:
  def spec = suite("Built-in test services")(
    test("ZIO Console") {
      val logicUnderTest: Task[Vector[String]] =
        for
          _      <- TestConsole.feedLines("Joe")
          _      <- DummyConsoleApp.welcomeUser
          output <- TestConsole.output
        yield output.map(_.trim)

      assertZIO(logicUnderTest)(equalTo(Vector("Name:", "Welcome, Joe!")))
    },
    test("ZIO clock") {
      val parallelEffect =
        for
          fiber  <- ZIO.sleep(5.minutes).timeout(1.minute).fork
          _      <- TestClock.adjust(1.minute)
          result <- fiber.join
        yield result

      assertZIO(parallelEffect)(isNone)
    },
    test("ZIO Random") {
      val effect =
        for
          _     <- TestRandom.feedInts(3, 4, 1, 2)
          value <- Random.nextInt
        yield value

      assertZIO(effect)(equalTo(3))
    }
  )

/**
 * Test aspects
 */
object AspectsSpec extends ZIOSpecDefault:

  def computeSth: UIO[Int] =
    ZIO.succeed(42).delay(2.seconds)

  def spec = suite("Testing aspects")(
    test("timeout aspect") {
      val eff =
        for
          molFib <- computeSth.fork
          _      <- TestClock.adjust(3.second)
          v      <- molFib.join
        yield v

      assertZIO(eff)(equalTo(42))
    } @@ timeout(10.millis)
      @@ eventually
  )
