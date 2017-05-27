package common

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.Matchers

trait MatrixMatchers extends Matchers {
  def assertMatricesEqual(m1: DenseMatrix[Double], m2: DenseMatrix[Double]): Unit = {
    m1.valuesIterator.zip(m2.valuesIterator).foreach { case (m1Value: Double, m2Value: Double) =>
      assert(roundAt2(m1Value) === roundAt2(m2Value))
    }
  }

  def assertVectorsEqual(v1: DenseVector[Double], v2: DenseVector[Double]): Unit = {
    v1.valuesIterator.zip(v2.valuesIterator).foreach { case (v1Value: Double, v2Value: Double) =>
      assert(roundAt2(v1Value) === roundAt2(v2Value))
    }

  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def roundAt2(p: Double) = roundAt(2)(p)
}
