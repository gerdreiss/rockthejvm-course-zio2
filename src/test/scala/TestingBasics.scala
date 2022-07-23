import zio.*
import zio.test.*
import zio.test.Assertion.*

case class Person(name: String, age: Int):
  def spellName: String  = name.toUpperCase()
  def speak: UIO[String] = ZIO.succeed(s"Hi, I'm $name")

object PersonTestSpec extends ZIOSpecDefault:

  val person = Person("John", 30)

  def spec = suite("Person tests")(
    test("Spelltest") {
      assert(person.spellName)(equalTo("JOHN"))
      assertTrue(person.spellName == "JOHN")
    },
    test("Speak") {
      assertZIO(person.speak)(equalTo("Hi, I'm John"))
      assertZIO(person.speak)(
        assertion("should be a greeting")(_ == "Hi, I'm John") &&
          assertion("should be an adult")(_ != "Hi, I'm Daniel")
      )
      // doesn't work with assertTrue
    },
    suite("nested suite")(
      test("Age") {
        assert(person.age)(equalTo(30))
      }
    )
  )
