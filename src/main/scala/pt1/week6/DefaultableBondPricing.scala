package pt1.week6

import breeze.linalg.{DenseMatrix, DenseVector}

object DefaultableBondPricing {
  def calculate(shortRateLattice: DenseMatrix[Double], hazardLattice: DenseMatrix[Double], faceValue: Double, recoveryRate: Double, q: Double, p: Double, maturity: Int): Double = {
    val n = maturity + 1
    val result = DenseMatrix.zeros[Double](n, n)

    val rateLattice = shortRateLattice + 1.0d
    val survivalLattice = (hazardLattice * -1.0d) + 1.0d
    val lastColumnIndex = maturity
    val recoveredValue = faceValue * recoveryRate
    result(::, lastColumnIndex) := faceValue

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val survivalColumn = survivalLattice(::, i)
      val hazardColumn = hazardLattice(::, i)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((survivalColumn :* ((previousColumnShiftedOneDown * q) + (previousColumn * p))) + (hazardColumn * recoveredValue))  / rateLattice(::, i)
      result(::, i) += currentColumn
    }
    result(0, 0)
  }

}
