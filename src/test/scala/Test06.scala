import org.scalatest.*
import assignments.Day06

class Test06 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day06.partOne() should be(4878670269096L)
    Day06.partTwo() should be(8674740488592L)
  }
