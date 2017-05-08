package pt1.week5

import breeze.linalg.{DenseMatrix, DenseVector}

object ForwardPricing {
  def calculate(shortRateLattice: DenseMatrix[Double], noCouponPricingMatrix: DenseMatrix[Double], cashAccountValue: Double, maturity: Int, q: Double, p: Double) = {
    val n = maturity + 1
    val result = DenseMatrix.zeros[Double](n, n)
    val rateLattice = shortRateLattice + 1.0d
    val lastColumnIndex = maturity
    result(::, lastColumnIndex) := noCouponPricingMatrix(::, lastColumnIndex).slice(0, n)

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((previousColumnShiftedOneDown * q) + (previousColumn * p)) / rateLattice(::, i)
      result(::, i) += currentColumn
    }
    result(0, 0) / cashAccountValue
  }

}
