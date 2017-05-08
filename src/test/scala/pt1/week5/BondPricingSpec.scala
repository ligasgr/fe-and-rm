package pt1.week5

import common.MatrixMatchers
import org.scalatest.FunSuite

class BondPricingSpec extends FunSuite with MatrixMatchers {

  val shortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 6)

  test("sample coupon bond price is as expected") {
    val price = BondPricing.calculate(shortRateLattice, faceValue = 100.0d, coupon = 0.1d, maturity = 6, q = 0.5d, p = 0.5d)

    price shouldBe 124.14 +- 0.01
  }
}
