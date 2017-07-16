package pt1.week4

import org.scalatest.{FunSuite, Matchers}

class EuropeanOptionPricingSpec extends FunSuite with Matchers {

  private val sharePriceLattice = SharePriceLattice.generate(initialPrice = 100.0, termInYears = 0.25, volatility = 0.3,
    numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01)

  test("Price should be as expected for put option") {
    val putOptionPrice = EuropeanOptionPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = true)
    val putOptionPriceBS = BlackScholesPricing.calculateCall(termInYears = 0.25, volatility = 0.3,
      interestRate = 0.02, initialPrice = 100.00, strikePrice = 110.00, dividendYield = 0.01, isPut = true)

    putOptionPrice shouldBe (12.31 +- 0.01)
    putOptionPriceBS shouldBe (12.26 +- 0.01)
  }

  test("Price should be as expected for call option") {
    val callOptionPrice = EuropeanOptionPricing.calculate(sharePriceLattice, termInYears = 0.25, volatility = 0.3,
      numberOfPeriods = 15, interestRate = 0.02, dividendYield = 0.01, strikePrice = 110.0, isPut = false)
    val callOptionPriceBS = BlackScholesPricing.calculateCall(termInYears = 0.25, volatility = 0.3,
      interestRate = 0.02, initialPrice = 100.00, strikePrice = 110.00, dividendYield = 0.01, isPut = false)

    callOptionPrice shouldBe (2.60 +- 0.01)
    callOptionPriceBS shouldBe (2.56 +- 0.01)
  }

  test("Price should be as expected for call option using Black-Scholes formula") {
    val callOptionPrice = BlackScholesPricing.calculateCall(termInYears = 0.5, volatility = 0.35, interestRate = 0.01,
      initialPrice = 23.75, strikePrice = 15.00, dividendYield = 0, isPut = false)

    callOptionPrice shouldBe (8.88 +- 0.01)
  }

  test("Price should be as expected for put option using Black-Scholes formula") {
    val putOptionPrice = BlackScholesPricing.calculateCall(termInYears = 0.5, volatility = 0.35, interestRate = 0.01,
      initialPrice = 23.75, strikePrice = 15.00, dividendYield = 0, isPut = true)

    putOptionPrice shouldBe (0.05 +- 0.01)
  }

  test("Price should be as expected for call option without dividends") {
    val sharePriceLattice = SharePriceLattice.generate(initialPrice = 23.75, termInYears = 0.5, volatility = 0.35,
      numberOfPeriods = 20, interestRate = 0.01, dividendYield = 0)

    val callOptionPrice = EuropeanOptionPricing.calculate(sharePriceLattice, termInYears = 0.5, volatility = 0.35,
      numberOfPeriods = 20, interestRate = 0.01, dividendYield = 0, strikePrice = 15.00, isPut = false)

    callOptionPrice shouldBe (8.88 +- 0.01)
  }

  test("Price should be as expected for put option without dividends") {
    val sharePriceLattice = SharePriceLattice.generate(initialPrice = 23.75, termInYears = 0.5, volatility = 0.35,
      numberOfPeriods = 20, interestRate = 0.01, dividendYield = 0)

    val putOptionPrice = EuropeanOptionPricing.calculate(sharePriceLattice, termInYears = 0.5, volatility = 0.35,
      numberOfPeriods = 20, interestRate = 0.01, dividendYield = 0, strikePrice = 15.00, isPut = true)

    putOptionPrice shouldBe (0.05 +- 0.01)
  }
}
