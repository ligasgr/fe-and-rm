package common

import breeze.linalg.{DenseMatrix, DenseVector}

object Propagation {

  def propagateValueForward(initialValue: Double, u: Double, d: Double, n: Int): DenseMatrix[Double] = {
    val result = DenseMatrix.zeros[Double](n, n)

    result.update(0, 0, initialValue)
    for (i <- 1 until result.cols) {
      val previousColumn = result(::, i - 1)
      val oneUp = previousColumn :* u
      val oneDown = DenseVector.fill(1, previousColumn.valueAt(0) * d)
      val currentColumn = DenseVector.vertcat(oneDown, oneUp(0 until oneUp.length - 1))
      result(::, i) += currentColumn
    }

    result
  }
}
