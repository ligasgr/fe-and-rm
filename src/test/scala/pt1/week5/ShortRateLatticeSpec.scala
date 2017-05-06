package pt1.week5

import breeze.linalg.DenseMatrix
import common.MatrixMatchers
import org.scalatest.FunSuite

class ShortRateLatticeSpec extends FunSuite with MatrixMatchers {

  test("lattice is as expected") {
    assertMatricesEqual(
      ShortRateLattice.generate(6.0d, 1.25d, 0.9d, 5),
      DenseMatrix(
        (6.00d, 5.40d, 4.86d, 4.37d, 3.94d, 3.54d),
        (0.00d, 7.50d, 6.75d, 6.08d, 5.47d, 4.92d),
        (0.00d, 0.00d, 9.38d, 8.44d, 7.59d, 6.83d),
        (0.00d, 0.00d, 0.00d, 11.72d, 10.55d, 9.49d),
        (0.00d, 0.00d, 0.00d, 0.00d, 14.65d, 13.18d),
        (0.00d, 0.00d, 0.00d, 0.00d, 0.00d, 18.31d)
      )
    )
  }
}
