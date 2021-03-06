package pt1.week5

import breeze.linalg.DenseMatrix
import common.Propagation

object ShortRateLattice {
  def generate(r0: Double, u: Double, d: Double, terms: Int): DenseMatrix[Double] = {
    Propagation.propagateValueForward(r0, u, d, terms + 1)
  }
}
