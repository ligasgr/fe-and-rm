package pt1.week6

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.exp

object BDT {

  def calculateRateLattice(a: DenseVector[Double], b: Double, q: Double, p: Double): DenseMatrix[Double] = {
    val n = a.length
    val indices = DenseVector((0 until n).map(_.toDouble).toArray)
    val result = DenseMatrix.zeros[Double](n, n)

    result(0 to 0, 0 to 0) := a(0)

    for (i <- 1 until n) {
      result(::, i) := exp(indices * b) * a(i)
      result(i + 1 until n, i) := 0.00d
    }

    result
  }
}
