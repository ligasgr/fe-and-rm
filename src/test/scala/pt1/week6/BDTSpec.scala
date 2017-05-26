package pt1.week6

import breeze.linalg.{DenseMatrix, DenseVector}
import common.MatrixMatchers
import org.scalatest.FunSuite

class BDTSpec extends FunSuite with MatrixMatchers {

  test("should calculate the lattice correctly") {
    val a = DenseVector.fill[Double](14, 5.00d)
    val b = 0.005d
    val result = BDT.calculateRateLattice(a, b, q = 0.5d, p = 0.5d)

    val expected = DenseMatrix(
      (5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d, 5.00d),
      (0.00d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d, 5.03d),
      (0.00d, 0.00d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d, 5.05d),
      (0.00d, 0.00d, 0.00d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d, 5.08d),
      (0.00d, 0.00d, 0.00d, 0.00d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d, 5.10d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.13d, 5.13d, 5.13d, 5.13d, 5.13d, 5.13d, 5.13d, 5.13d, 5.13d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.15d, 5.15d, 5.15d, 5.15d, 5.15d, 5.15d, 5.15d, 5.15d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.18d, 5.18d, 5.18d, 5.18d, 5.18d, 5.18d, 5.18d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.20d, 5.20d, 5.20d, 5.20d, 5.20d, 5.20d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.23d, 5.23d, 5.23d, 5.23d, 5.23d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.26d, 5.26d, 5.26d, 5.26d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.28d, 5.28d, 5.28d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.31d, 5.31d),
      (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 5.34d)
    )
    assertMatricesEqual(expected, result)
  }
}
