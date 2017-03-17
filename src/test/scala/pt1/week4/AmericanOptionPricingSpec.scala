package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class AmericanOptionPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for put option") {
    val putOptionPrice = AmericanOptionPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = true)

    putOptionPrice shouldBe (12.36 +- 0.01)
  }

  test("Price should be as expected for call option") {
    val callOptionPrice = AmericanOptionPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = false)

    callOptionPrice shouldBe (2.60 +- 0.01)
  }

  test("earliest optimal for put option") {
    val earliest = AmericanOptionPricing.earliest(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = true)

    earliest shouldBe 6
  }
}
