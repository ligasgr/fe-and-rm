package pt1.week6

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.pow
import breeze.optimize.{ApproximateGradientFunction, LBFGS}

object Calibration {

  def calibrate(initialParams: DenseVector[Double], valuesToCompareTo: DenseVector[Double])
               (valueComputation: DenseVector[Double] => DenseVector[Double]): DenseVector[Double] = {
    val lbfgs = new LBFGS[DenseVector[Double]](m = 7, tolerance = 1E-10)
    val calculation: DenseVector[Double] => Double = { x: DenseVector[Double] =>
      val computedValue = valueComputation(x)
      sum(pow(computedValue - valuesToCompareTo, 2.0d))
    }

    val diffFunction = new ApproximateGradientFunction(calculation)

    lbfgs.minimize(diffFunction, initialParams)

  }
}
