package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class EuropeanChooserOptionPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for call option") {
    val chooserOptionPrice = EuropeanOptionPricing.calculateChooser(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 100.0, choosingPeriod = 10)


    chooserOptionPrice shouldBe (10.81 +- 0.01)
  }
}
