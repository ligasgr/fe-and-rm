package pt1.week5

import breeze.linalg.{*, DenseMatrix, DenseVector, sum}
import breeze.numerics.pow

object ForwardEquations {
  def calculate(faceValue: Double, shortRateLattice: DenseMatrix[Double], q: Double, p: Double): (DenseVector[Double], DenseVector[Double]) = {
    val extendedRateLattice = extendOneUp(shortRateLattice)
    val rateLattice = extendedRateLattice + 1.00d
    val shiftedRateLattice = shiftOneUp(extendedRateLattice) + 1.00d
    val n = shortRateLattice.cols + 1
    val elementaryPrices = DenseMatrix.zeros[Double](shortRateLattice.rows + 1, n)
    elementaryPrices(0, 0) = 1.00d

    for (i <- 1 until n) {
      val previousColumn = elementaryPrices(::, i - 1)
      val previousColumnShiftedOneUp = DenseVector.vertcat(DenseVector.zeros[Double](1), previousColumn(0 until previousColumn.length - 1))
      val rateColumn = rateLattice(::, i - 1)
      val shiftedRateColumn = shiftedRateLattice(::, i - 1)
      val shiftedComponent = previousColumnShiftedOneUp :/ shiftedRateColumn
      val regularComponent = previousColumn :/ rateColumn
      elementaryPrices(::, i) := (shiftedComponent * q) + (regularComponent * p)
    }

    val sums = sum(elementaryPrices(::, *)).t
    val prices = sums(1 until sums.length) * faceValue
    val indices = DenseVector((0 until sums.length).map(_.toDouble).toArray)
    val spotRates = pow(DenseVector.ones[Double](sums.length) :/ sums, DenseVector.ones[Double](sums.length) :/ indices) - 1.00d
    (prices , spotRates(1 until spotRates.length))
  }

  private def extendOneUp(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val result = DenseMatrix.zeros[Double](m.rows + 1, m.cols)

    for (i <- 0 until m.cols) {
      result(::, i) := DenseVector.vertcat(m(::, i), DenseVector.zeros[Double](1))
    }

    result
  }

  private def shiftOneUp(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val result = DenseMatrix.zeros[Double](m.rows, m.cols)

    for (i <- 0 until m.cols) {
      val column = m(::, i)
      result(::, i) := DenseVector.vertcat(DenseVector.zeros[Double](1), column(0 until column.length - 1))
    }

    result
  }

}
