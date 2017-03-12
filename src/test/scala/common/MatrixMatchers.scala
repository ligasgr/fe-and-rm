package common

import breeze.linalg.DenseMatrix
import org.scalatest.Matchers

trait MatrixMatchers extends Matchers {
  def assertMatricesEqual(m1: DenseMatrix[Double], m2: DenseMatrix[Double]): Unit = {
    m1.valuesIterator.zip(m2.valuesIterator).foreach { case (m1Value: Double, m2Value: Double) =>
      assert(roundAt2(m1Value) === roundAt2(m2Value))
    }
  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def roundAt2(p: Double) = roundAt(2)(p)
}
