package pt1.week5

import breeze.linalg.{DenseMatrix, DenseVector, max}

object SwapPricing {
  def calculate(shortRateLattice: DenseMatrix[Double], notional: Double, fixedRate: Double, startingAt: Int = 1, terms: Int, q: Double, p: Double): Double = {
    val result: DenseMatrix[Double] = calculatePricingMatrix(shortRateLattice, fixedRate, startingAt, terms, q, p)
    result(0, 0) * notional
  }

  def calculateOption(shortRateLattice: DenseMatrix[Double], notional: Double, fixedRate: Double, startingAt: Int = 1, terms: Int, expiration: Int, q: Double, p: Double): Double = {
    val rateLattice = shortRateLattice + 1.0d
    val swapMatrix: DenseMatrix[Double] = calculatePricingMatrix(shortRateLattice, fixedRate, startingAt, terms, q, p)
    val n = expiration + 1
    val result = DenseMatrix.zeros[Double](n, n)
    val zero = DenseVector.zeros[Double](n)
    val lastColumnIndex = expiration
    result(::, lastColumnIndex) := max(swapMatrix(::, lastColumnIndex).slice(0, n), zero)

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((previousColumnShiftedOneDown * q) + (previousColumn * p)) / rateLattice(::, i)
      result(::, i) += currentColumn
    }

    result(0, 0) * notional
  }

  private def calculatePricingMatrix(shortRateLattice: DenseMatrix[Double], fixedRate: Double, startingAt: Int, terms: Int, q: Double, p: Double) = {
    val rateLattice = shortRateLattice + 1.0d
    val n = terms
    val startingAtColumn = startingAt - 1
    val result = DenseMatrix.zeros[Double](n, n)
    val lastColumnIndex = terms - 1
    result(::, lastColumnIndex) := (shortRateLattice(::, lastColumnIndex) - fixedRate) / rateLattice(::, lastColumnIndex)

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val valueInCurrentColumn = if (i >= startingAtColumn)
        (shortRateLattice(::, i) - fixedRate) + (previousColumnShiftedOneDown * q) + (previousColumn * p)
      else
        (previousColumnShiftedOneDown * q) + (previousColumn * p)
      val currentColumn = valueInCurrentColumn / rateLattice(::, i)
      result(::, i) += currentColumn
    }
    result
  }
}
