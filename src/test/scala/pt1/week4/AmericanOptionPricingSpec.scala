package pt1.week4

import org.scalatest.{FunSuite, Matchers}
import pt1.week5.{BondPricing, ShortRateLattice}

class AmericanOptionPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for put option") {
    val putOptionPrice = AmericanOptionPricing.calculateForShares(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = true)

    putOptionPrice shouldBe (12.36 +- 0.01)
  }

  test("Price should be as expected for call option") {
    val callOptionPrice = AmericanOptionPricing.calculateForShares(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = false)

    callOptionPrice shouldBe (2.60 +- 0.01)
  }

  test("earliest optimal for put option") {
    val earliest = AmericanOptionPricing.earliestExercise(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = true)

    earliest shouldBe 5
  }

  test("Price for option on bond should be as expected") {
    val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)
    val bondPricingMatrix = BondPricing.calculatePricingMatrix(shortRateLattice, faceValue = 100.0d, coupon = 0.0d, maturity = 10, q = 0.5d, p = 0.5d)
    val callOptionPrice = AmericanOptionPricing.calculate(shortRateLattice, bondPricingMatrix, strikePrice = 80.0, isPut = false, 6, 0.5, 0.5)

    callOptionPrice shouldBe (2.36 +- 0.01)
  }
}
