package pt1.week6

import breeze.linalg.DenseMatrix
import common.MatrixMatchers
import org.scalatest.FunSuite
import pt1.week5.ShortRateLattice

class DefaultableBondPricingSpec extends FunSuite with MatrixMatchers {

  test("Price is as expected") {
    val shortRateLattice = ShortRateLattice.generate(r0 = 0.05d, u = 1.1d, d = 0.9d, terms = 10)
    val hazardLattice = generateHazardLattice()
    val faceValue = 100.00d
    val recoveryRate = 0.2d
    val price = DefaultableBondPricing.calculate(shortRateLattice, hazardLattice, faceValue, recoveryRate, q = 0.5d, p = 0.5d, 10)

    price shouldBe 57.22d +- 0.01
  }

  private def generateHazardLattice() = {
    val a = 0.01d
    val b = 1.01d
    val result = DenseMatrix.zeros[Double](11, 11)

    for (i <- 0 until result.cols; j <- 0 to i) {
      val value = a * math.pow(b, j - (i / 2.0))
      result.update(j, i, value)
    }

    result
  }
}
