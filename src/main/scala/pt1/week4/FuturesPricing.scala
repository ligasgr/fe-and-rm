package pt1.week4

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.math.{exp, sqrt}

object FuturesPricing {
  def calculate(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double,
                numberOfPeriods: Int, interestRate: Double, dividendYield: Double): Double = {
    val result = calculatePricingMatrix(sharePriceLattice, termInYears, volatility, numberOfPeriods, interestRate, dividendYield)

    result.apply(0, 0)
  }

  def calculatePricingMatrix(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double, numberOfPeriods: Int, interestRate: Double, dividendYield: Double) = {
        val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
        val d: Double = 1 / u
        val n = numberOfPeriods + 1
        val q: Double = (exp((interestRate - dividendYield) * termInYears / numberOfPeriods) - d) / (u - d)
        val p: Double = 1 - q

        val result = DenseMatrix.zeros[Double](n, n)
        val lastColumnIndex = numberOfPeriods
        result(::, lastColumnIndex) := sharePriceLattice(::, lastColumnIndex)

        val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
        for (i <- columnsFromPriorToLastOneDownToFirst) {
          val previousColumn = result(::, i + 1)
          val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
          val currentColumn = (previousColumnShiftedOneDown * q) + (previousColumn * p)
          result(::, i) += currentColumn
        }
        result
      }
}
