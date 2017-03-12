package pt1.week4

import breeze.linalg.DenseMatrix
import common.Propagation

import scala.math.{exp, sqrt}

object SharePriceLattice {

// for option pricing
//  val q: Double = exp((interestRate - dividendYield) * termInYears / numberOfPeriods) / (u - d)
//  val p: Double = 1 - q

  def generate(initialPrice: Double, termInYears: Double, volatility: Double,
               numberOfPeriods: Int, interestRate: Double, dividendYield: Double): DenseMatrix[Double] = {
    val u: Double = exp(volatility * sqrt(termInYears / numberOfPeriods))
    val d: Double = 1 / u
    val n = numberOfPeriods + 1
    Propagation.propagateValueForward(initialPrice, u, d, n)
  }
}
