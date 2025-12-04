import org.scalatest.*
import assignments.Day02

class Test02 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day02.partOne() should be(8576933996L)
    Day02.partTwo() should be(25663320831L)
  }
