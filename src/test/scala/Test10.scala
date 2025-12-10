import org.scalatest.*
import assignments.Day10

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "solve example correctly" in {
    Day10.partOne() should be(535)
    Day10.partTwo() should be(21021)
  }

