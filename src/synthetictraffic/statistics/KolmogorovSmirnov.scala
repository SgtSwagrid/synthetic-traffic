package synthetictraffic.statistics

import scala.math._

object KolmogorovSmirnov {

  def error(d1: DiscreteDistribution, d2: DiscreteDistribution): Double = {

    val start = min(d1.min, d2.min)
    val end = max(d1.max, d2.max)

    (start to end).map { n =>

      val cp1 = d1.cumulativeProbability(n)
      val cp2 = d2.cumulativeProbability(n)

      abs(cp2 - cp1)

    }.max
  }
}