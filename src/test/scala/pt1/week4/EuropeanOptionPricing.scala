package pt1.week4

import breeze.linalg.{DenseMatrix, DenseVector, _}

import scala.math.{exp, sqrt}

object EuropeanOptionPricing {

  def calculate(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double,
                numberOfPeriods: Int, interestRate: Double, dividendYield: Double, strikePrice: Double, isPut: Boolean): Double = {
    val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
    val d: Double = 1 / u
    val n = numberOfPeriods + 1
    val q: Double = (exp((interestRate - dividendYield) * termInYears / numberOfPeriods) - d) / (u - d)
    val p: Double = 1 - q
    val periodRelatedDivisor: Double = exp(interestRate * termInYears / numberOfPeriods)
    val putCallMultiplier: Double = if (isPut) -1 else 1

    val result = DenseMatrix.zeros[Double](n, n)
    val lastColumnIndex = numberOfPeriods
    val newLastRow = (sharePriceLattice(::, lastColumnIndex) - strikePrice) * putCallMultiplier
    val finalLastRow = max(result(::, lastColumnIndex), newLastRow)
    result(::, lastColumnIndex) := finalLastRow

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((previousColumnShiftedOneDown * q) + (previousColumn * p)) / periodRelatedDivisor
      result(::, i) += currentColumn
    }

    result.apply(0, 0)
  }
}
