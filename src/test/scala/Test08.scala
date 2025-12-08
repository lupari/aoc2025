import org.scalatest.*
import assignments.Day08

class Test08 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day08.partOne() should be(123930)
    Day08.partTwo() should be(27338688)
  }
