package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class AmericanOptionOnFuturesPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for call option") {
    val futuresLattice = FuturesPricing.calculatePricingMatrix(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)
    val callOptionPrice = AmericanOptionPricing.calculate(futuresLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = false)(exercisePeriod = 10)

    callOptionPrice shouldBe (1.66 +- 0.01)
  }

}
