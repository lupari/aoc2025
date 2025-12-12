import assignments.Day12
import org.scalatest.*

class Test12 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "solve example correctly" in {
    Day12.partOne() should be(433)
  }

