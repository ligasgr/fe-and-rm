package pt1.week5

import common.MatrixMatchers
import org.scalatest.FunSuite

class ForwardPricingSpec extends FunSuite with MatrixMatchers {

  test("forward price should be as expected") {
    val faceValue = 100.0d
    val coupon = 0.1d
    val shortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 6)
    val cashAccountValue = BondPricing.calculate(shortRateLattice, 1.0d, 0.0d, 4, 0.5d, 0.5d)
    val bondPricingMatrix = BondPricing.calculatePricingMatrix(shortRateLattice, faceValue, coupon, maturity = 6, q = 0.5d, p = 0.5d)
    val noCouponPricingMatrix = bondPricingMatrix - (faceValue * coupon)
    val price = ForwardPricing.calculate(shortRateLattice, noCouponPricingMatrix, cashAccountValue, maturity = 4, q = 0.5d, p = 0.5d)

    price shouldBe 103.38 +- 0.01
  }

  test("zero coupon bond forward price is as expected") {
    val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)
    val cashAccountValue = BondPricing.calculate(shortRateLattice, 1.0d, 0.0d, 4, 0.5d, 0.5d)
    val bondPricingMatrix = BondPricing.calculatePricingMatrix(shortRateLattice, faceValue = 100.0d, coupon = 0.0d, maturity = 10, q = 0.5d, p = 0.5d)
    val price = ForwardPricing.calculate(shortRateLattice, bondPricingMatrix, cashAccountValue, maturity = 4, q = 0.5d, p = 0.5d)

    price shouldBe 74.88 +- 0.01
  }
}
