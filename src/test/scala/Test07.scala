import org.scalatest.*
import assignments.Day07

class Test07 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day07.partOne() should be(1609)
    Day07.partTwo() should be(12472142047197L)
  }
