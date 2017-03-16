package pt1.week4

import breeze.linalg.{DenseMatrix, DenseVector, _}

import scala.math.{exp, sqrt}

object EuropeanOptionPricing {

  def calculate(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double,
                numberOfPeriods: Int, interestRate: Double, dividendYield: Double, strikePrice: Double): Double = {
    val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
    val d: Double = 1 / u
    val n = numberOfPeriods + 1
    val q: Double = exp((interestRate - dividendYield) * termInYears / numberOfPeriods) / (u - d)
    val p: Double = 1 - q
    val isPut = true
    val putCallMultiplier: Double = if (isPut) -1 else 1

    val result = DenseMatrix.zeros[Double](n, n)
    val lastColumnIndex = numberOfPeriods

    val newLastRow = (sharePriceLattice(::, lastColumnIndex) - strikePrice) * putCallMultiplier
    val finalLastRow = max(result(::, lastColumnIndex), newLastRow)
    result(::, lastColumnIndex) := finalLastRow

    for (i <- lastColumnIndex - 1 to 0) {
      val previousColumn = result(::, i + 1)
      val oneUp = previousColumn :* u
      val oneDown = DenseVector.fill(1, previousColumn.valueAt(0) * d)
      val currentColumn = DenseVector.vertcat(oneDown, oneUp(0 until oneUp.length - 1))
      result(::, i) += currentColumn
    }

    result.apply(0, 0)
  }
}
