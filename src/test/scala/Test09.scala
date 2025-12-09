import org.scalatest.*
import assignments.Day09

class Test09 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day09.partOne() should be(4765757080L)
    Day09.partTwo() should be(1498673376)
  }
