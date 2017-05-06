package pt1.week5

import common.MatrixMatchers
import org.scalatest.FunSuite

class ZeroCouponBondPricingSpec extends FunSuite with MatrixMatchers {

  val sampleShortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 5)
  val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)

  test("sample zero coupon bond price is as expected") {
    val price = ZeroCouponBondPricing.calculate(sampleShortRateLattice, faceValue = 100.0d, maturity = 4, q = 0.5d, p = 0.5d)

    price shouldBe 77.22 +- 0.01
  }

  test("zero coupon bond price is as expected") {
    val price = ZeroCouponBondPricing.calculate(shortRateLattice, faceValue = 100.0d, maturity = 10, q = 0.5d, p = 0.5d)

    price shouldBe 61.62 +- 0.01
  }
}
