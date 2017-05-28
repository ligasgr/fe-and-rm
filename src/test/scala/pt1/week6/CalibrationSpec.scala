package pt1.week6

import breeze.linalg.DenseVector
import common.MatrixMatchers
import org.scalatest.FunSuite
import pt1.week5.{ForwardEquations, SwapPricing}

class CalibrationSpec extends FunSuite with MatrixMatchers {

  test("calibrated parameters should give expected rates") {
    val initialA = DenseVector.fill[Double](10, 3.00d)
    val b = 0.05d
    val observedMarketRates = DenseVector(3.00d, 3.1d, 3.2d, 3.3d, 3.4d, 3.5d, 3.55d, 3.60d, 3.65d, 3.7d)

    val optimizedA = Calibration.calibrate(initialA, observedMarketRates)(bdtForwardEquations(b))

    assertVectorsEqual(bdtForwardEquations(b)(optimizedA), observedMarketRates)
  }

  private def bdtForwardEquations(b: Double) = { a: DenseVector[Double] =>
    val bdt = BDT.calculateRateLattice(a, b, q = 0.5d, p = 0.5d)
    val (_, spotRates) = ForwardEquations.calculate(1.0d, bdt / 100.0d, q = 0.5d, p = 0.5d)
    spotRates * 100.00d
  }

  test("swaption price for calibrated model should be as expected") {
    val initialA = DenseVector.fill[Double](10, 3.00d)
    val b = 0.05d
    val observedMarketRates = DenseVector(3.00d, 3.1d, 3.2d, 3.3d, 3.4d, 3.5d, 3.55d, 3.60d, 3.65d, 3.7d)
    val optimizedA = Calibration.calibrate(initialA, observedMarketRates)(bdtForwardEquations(b))

    val notional = 1000000.0d
    val fixedRate = 0.039d
    val shortRateLattice = BDT.calculateRateLattice(optimizedA, b, q = 0.5d, p = 0.5d)
    val price = SwapPricing.calculateOption(shortRateLattice / 100.00d, notional, fixedRate, terms = 10, expiration = 3, q = 0.5d, p = 0.5d)

    price shouldBe 4102.00d +- 1.00
  }

  test("swaption price for calibrated model with different b should be as expected") {
    val initialA = DenseVector.fill[Double](10, 3.00d)
    val b = 0.1d
    val observedMarketRates = DenseVector(3.00d, 3.1d, 3.2d, 3.3d, 3.4d, 3.5d, 3.55d, 3.60d, 3.65d, 3.7d)
    val optimizedA = Calibration.calibrate(initialA, observedMarketRates)(bdtForwardEquations(b))

    val notional = 1000000.0d
    val fixedRate = 0.039d
    val shortRateLattice = BDT.calculateRateLattice(optimizedA, b, q = 0.5d, p = 0.5d)
    val price = SwapPricing.calculateOption(shortRateLattice / 100.00d, notional, fixedRate, terms = 10, expiration = 3, q = 0.5d, p = 0.5d)

    price shouldBe 8096.00d +- 1.00
  }

}
