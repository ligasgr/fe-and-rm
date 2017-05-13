package pt1.week5

import common.MatrixMatchers
import org.scalatest.FunSuite

class SwapPricingSpec extends FunSuite with MatrixMatchers {
  test("swap price should be as expected") {
    val notional = 1.0d
    val fixedRate = 0.05d
    val shortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 5)
    val price = SwapPricing.calculate(shortRateLattice, notional, fixedRate, terms = 6, q = 0.5d, p = 0.5d)

    price shouldBe 0.0990 +- 0.0001
  }

  test("forward starting swap price is as expected") {
    val notional = 1000000d
    val fixedRate = 0.045d
    val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)
    val price = SwapPricing.calculate(shortRateLattice, notional, fixedRate, startingAt = 2, terms = 11, q = 0.5d, p = 0.5d)

    price shouldBe 33374.00 +- 1.0
  }

  test("swaption price should be as expected") {
    val notional = 1.0d
    val fixedRate = 0.05d
    val shortRateLattice = ShortRateLattice.generate(0.06d, 1.25d, 0.9d, 5)
    val price = SwapPricing.calculateOption(shortRateLattice, notional, fixedRate, terms = 6, expiration = 3, q = 0.5d, p = 0.5d)

    price shouldBe 0.0620 +- 0.0001
  }

  test("price of swaption on forward starting swap should be as expected") {
    val notional = 1000000d
    val fixedRate = 0.045d
    val shortRateLattice = ShortRateLattice.generate(0.05d, 1.1d, 0.9d, 10)
    val price = SwapPricing.calculateOption(shortRateLattice, notional, fixedRate, startingAt = 2, terms = 11, expiration = 5, q = 0.5d, p = 0.5d)

    price shouldBe 26311.0 +- 1.0
  }
}
