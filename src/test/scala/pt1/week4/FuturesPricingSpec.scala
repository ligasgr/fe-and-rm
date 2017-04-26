package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class FuturesPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for futures") {
    val futuresPrice = FuturesPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

    futuresPrice shouldBe (100.25 +- 0.01)
  }
}
