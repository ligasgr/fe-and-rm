package pt1.week4

import breeze.linalg.{DenseMatrix, DenseVector, _}

import scala.math.{exp, sqrt}

object EuropeanOptionPricing {

  def calculate(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double,
                numberOfPeriods: Int, interestRate: Double, dividendYield: Double, strikePrice: Double, isPut: Boolean): Double = {
    val result = calculatePricingMatrix(sharePriceLattice, termInYears, volatility, numberOfPeriods, interestRate, dividendYield, strikePrice, isPut)

    result.apply(0, 0)
  }

  def calculateChooser(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double,
                numberOfPeriods: Int, interestRate: Double, dividendYield: Double, strikePrice: Double, choosingPeriod: Int): Double = {
    val pricingPut = calculatePricingMatrix(sharePriceLattice, termInYears, volatility, numberOfPeriods, interestRate, dividendYield, strikePrice, true)
    val pricingCall = calculatePricingMatrix(sharePriceLattice, termInYears, volatility, numberOfPeriods, interestRate, dividendYield, strikePrice, false)

    val maxOfPutAndCall = max(pricingPut(::, choosingPeriod), pricingCall(::, choosingPeriod))

    val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
    val d: Double = 1 / u
    val q: Double = (exp((interestRate - dividendYield) * termInYears / numberOfPeriods) - d) / (u - d)
    val p: Double = 1 - q
    val periodRelatedDivisor: Double = exp(interestRate * termInYears / numberOfPeriods)

    val result = DenseMatrix.zeros[Double](choosingPeriod + 1, choosingPeriod + 1)
    val lastColumnIndex = choosingPeriod
    result(::, lastColumnIndex) := maxOfPutAndCall.slice(0, choosingPeriod + 1)

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((previousColumnShiftedOneDown * q) + (previousColumn * p)) / periodRelatedDivisor
      result(::, i) += currentColumn
    }

    result.apply(0, 0)
  }

  private def calculatePricingMatrix(sharePriceLattice: DenseMatrix[Double], termInYears: Double, volatility: Double, numberOfPeriods: Int, interestRate: Double, dividendYield: Double, strikePrice: Double, isPut: Boolean) = {
    val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
    val d: Double = 1 / u
    val n = numberOfPeriods + 1
    val q: Double = (exp((interestRate - dividendYield) * termInYears / numberOfPeriods) - d) / (u - d)
    val p: Double = 1 - q
    val periodRelatedDivisor: Double = exp(interestRate * termInYears / numberOfPeriods)
    val putCallMultiplier: Double = if (isPut) -1 else 1

    val result = DenseMatrix.zeros[Double](n, n)
    val zero = DenseVector.zeros[Double](n)
    val lastColumnIndex = numberOfPeriods
    result(::, lastColumnIndex) := profitAtTheMoment(zero, sharePriceLattice, strikePrice, putCallMultiplier, lastColumnIndex)

    val columnsFromPriorToLastOneDownToFirst = (lastColumnIndex - 1) to 0 by -1
    for (i <- columnsFromPriorToLastOneDownToFirst) {
      val previousColumn = result(::, i + 1)
      val previousColumnShiftedOneDown = DenseVector.vertcat(previousColumn(1 until previousColumn.length), DenseVector.zeros[Double](1))
      val currentColumn = ((previousColumnShiftedOneDown * q) + (previousColumn * p)) / periodRelatedDivisor
      result(::, i) += currentColumn
    }
    result
  }

  private def profitAtTheMoment(zero: DenseVector[Double], sharePriceLattice: DenseMatrix[Double],
                                strikePrice: Double, putCallMultiplier: Double, columnIndex: Int) = {
    max(zero, (sharePriceLattice(::, columnIndex) - strikePrice) * putCallMultiplier)
  }
}
