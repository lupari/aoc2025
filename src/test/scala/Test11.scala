import assignments.Day11
import org.scalatest.*

class Test11 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "solve example correctly" in {
    Day11.partOne() should be(508)
    Day11.partTwo() should be(315116216513280L)
  }

