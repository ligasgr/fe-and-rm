package pt1.week4

import org.scalatest.{FunSuite, Matchers}
import pt1.week5.{BondPricing, ShortRateLattice}

class FuturesPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for futures") {
    val futuresPrice = FuturesPricing.calculateForShares(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

    futuresPrice shouldBe (100.25 +- 0.01)
  }

  test("Price should be as expected for futures on bond") {
    val faceValue = 100.0d
    val coupon = 0.1d
    val shortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 6)
    val bondPricingMatrix = BondPricing.calculatePricingMatrix(shortRateLattice, faceValue, coupon, maturity = 6, q = 0.5d, p = 0.5d)
    val noCouponPricingMatrix = bondPricingMatrix - (faceValue * coupon)
    val futuresPrice = FuturesPricing.calculate(noCouponPricingMatrix, 4, 0.5d, 0.5d)

    futuresPrice shouldBe (103.22 +- 0.01)
  }

  test("Price should be as expected for futures on zero-coupon bond") {
    val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)
    val bondPricingMatrix = BondPricing.calculatePricingMatrix(shortRateLattice, faceValue = 100.0d, coupon = 0.0d, maturity = 10, q = 0.5d, p = 0.5d)
    val futuresPrice = FuturesPricing.calculate(bondPricingMatrix, 4, 0.5d, 0.5d)

    futuresPrice shouldBe (74.82 +- 0.01)
  }
}
