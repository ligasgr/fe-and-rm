package pt1.week5

import breeze.linalg.DenseVector
import common.MatrixMatchers
import org.scalatest.FunSuite

class ForwardEquationsSpec extends FunSuite with MatrixMatchers {

  test("has elementary prices and interest rates as expected for forward equations") {
    val shortRateLattice = ShortRateLattice.generate(r0 = 0.06d, u = 1.25d, d = 0.9d, terms = 5)
    val (prices, spotRates) = ForwardEquations.calculate(faceValue = 100.00d, shortRateLattice, q = 0.5d, p = 0.5d)

    assertVectorsEqual(prices, DenseVector(94.34d, 88.63d, 82.91d, 77.22d, 71.59d, 66.06d))
    assertVectorsEqual(spotRates * 100.00d, DenseVector(6.00d, 6.22d, 6.45d, 6.68d, 6.91d, 7.15d))
  }
}
