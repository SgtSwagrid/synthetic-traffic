package synthetictraffic.statistics

import scala.math.{floor, pow}

object BenfordDistribution {

  def withBase(base: Int): DiscreteDistribution = {

    val distribution = (1 until base).map { digit => digit ->
      log(1.0 + 1.0 / digit, base)
    }.toMap

    new DiscreteDistribution(distribution)
  }

  def firstDigit(n: Int, b: Int): Int = {
    floor(n / pow(b, floor(log(n, b)))).toInt
  }

  private def log(n: Double, b: Int): Double = {
    Math.log(n) / Math.log(b)
  }
}