package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class EuropeanOptionPricingSpec extends FunSuite with Matchers {

  test("Price should be as expected for put option") {
    val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

    val putOptionPrice = EuropeanOptionPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0)

    putOptionPrice shouldBe (12.31 +- 0.01)
  }
}
