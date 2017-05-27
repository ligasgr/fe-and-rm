package pt1.week6

import breeze.linalg.DenseVector
import common.MatrixMatchers
import org.scalatest.FunSuite
import pt1.week5.ForwardEquations

class CalibrationSpec extends FunSuite with MatrixMatchers {

  test("calibrated parameters should give expected rates") {
    val initialA = DenseVector.fill[Double](10, 0.03d)
    val b = 0.05d
    val observedMarketRates = DenseVector(0.03d, 0.031d, 0.032d, 0.033d, 0.034d, 0.035d, 0.0355d, 0.0360d, 0.0365d, 0.037d)

    val optimizedA = Calibration.calibrate(initialA, observedMarketRates)(bdtForwardEquations(b))

    assertVectorsEqual(bdtForwardEquations(b)(optimizedA), observedMarketRates)
  }

  private def bdtForwardEquations(b: Double) = { a: DenseVector[Double] =>
    val bdt = BDT.calculateRateLattice(a, b, q = 0.5d, p = 0.5d)
    val (_, spotRates) = ForwardEquations.calculate(1.0d, bdt, q = 0.5d, p = 0.5d)
    spotRates
  }

}
