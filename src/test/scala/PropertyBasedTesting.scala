import zio.*
import zio.test.*
import zio.test.Assertion.*

object PropertyBasedTesting extends ZIOSpecDefault:

  val intGenerator                = Gen.int
  val charGenerator               = Gen.char
  val stringGenerator             = Gen.string
  val cappedLengthStringGenerator = Gen.stringN(10)(Gen.alphaNumericChar)
  val constGenerator              = Gen.const("Scala")
  val valuesGenerator             = Gen.elements(1, 2, 3, 4, 5, 6, 7)
  val valuesIterableGenerator     = Gen.fromIterable(1 to 1000)
  val uniformDoublesGenerator     = Gen.uniform // doubles between 0 and 1
  // etc. pp.

  def spec = test("property based testing basics") {
    check(Gen.int, Gen.int, Gen.int) { (a, b, c) =>
      assertTrue((a + b) + c == a + (b + c))
    }
  }
