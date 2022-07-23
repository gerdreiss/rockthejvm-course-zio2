import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.junit.*

class JUnitCompatSpec extends JUnitRunnableSpec:

  case class Person(name: String, age: Int):
    def spellName: String  = name.toUpperCase()
    def speak: UIO[String] = ZIO.succeed(s"Hi, I'm $name")

  val person = Person("John", 30)

  def spec = suite("JUnitCompatSuite")(
    test("JUnitCompatSpeak") {
      assertZIO(person.speak)(
        assertion("should be a greeting")(_ == "Hi, I'm John") &&
          assertion("should be an adult")(_ != "Hi, I'm Daniel")
      )
    },
    test("JUnitCompatAge") {
      assert(person.age)(equalTo(30))
    }
  )
