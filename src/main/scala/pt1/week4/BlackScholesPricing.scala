package pt1.week4

import breeze.stats.distributions.Gaussian

/**
  * Mostly based on http://introcs.cs.princeton.edu/java/22library/BlackScholes.java
  * Put price and dividend inclusion from http://www.macroption.com/black-scholes-formula/
  */
object BlackScholesPricing {
  val standardDistribution = new Gaussian(0, 1)

  def calculateCall(termInYears: Double, volatility: Double, interestRate: Double,
                    initialPrice: Double, strikePrice: Double, dividendYield: Double, isPut: Boolean): Double = {
    if (isPut) putPrice(initialPrice, strikePrice, interestRate, dividendYield, volatility, termInYears)
    else callPrice(initialPrice, strikePrice, interestRate, dividendYield, volatility, termInYears)
  }

  private def callPrice(s: Double, x: Double, r: Double, q: Double, sigma: Double, t: Double): Double = {
    val d1 = d1f(s, x, r, q, sigma, t)
    val d2 = d2f(d1, sigma, t)
    s * Math.exp(-q * t) * standardDistribution.cdf(d1) - x * Math.exp(-r * t) * standardDistribution.cdf(d2)
  }

  private def putPrice(s: Double, x: Double, r: Double, q: Double, sigma: Double, t: Double): Double = {
    val d1 = d1f(s, x, r, q, sigma, t)
    val d2 = d2f(d1, sigma, t)
    x * Math.exp(-r * t) * standardDistribution.cdf(-d2) - s * Math.exp(-q * t) * standardDistribution.cdf(-d1)
  }

  private def d1f(s: Double, x: Double, r: Double, q: Double, sigma: Double, t: Double): Double = {
    (Math.log(s / x) + (r - q + sigma * sigma / 2) * t) / (sigma * Math.sqrt(t))
  }

  private def d2f(d1: Double, sigma: Double, t: Double) = {
    d1 - sigma * Math.sqrt(t)
  }
}
