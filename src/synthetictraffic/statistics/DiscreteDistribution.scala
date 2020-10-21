package synthetictraffic.statistics

class DiscreteDistribution(val distribution: Map[Int, Double]) {

  def probability(n: Int): Double = {
    distribution.get(n).getOrElse(0)
  }

  def cumulativeProbability(n: Int): Double = {
    if(n < min) 0.0 else if(n > max) 1.0 else cumulativeDistribution(n)
  }

  lazy val values = distribution.keys.toSeq.sorted
  lazy val min = distribution.keys.min
  lazy val max = distribution.keys.max

  lazy val cumulativeDistribution = {

    val probabilities = values.sliding(2).flatMap {
      case Seq(n1, n2) =>
        probability(n1) +: List.fill(n2 - n1 - 1)(0.0)
      case Seq(_) => List()
    }.scanLeft(0.0)(_ + _).toSeq :+ 1.0

    (min to max).zip(probabilities).toMap
  }

  override def toString(): String = {
    values.map(v => v -> probability(v)).mkString("\n")
  }
}

object DiscreteDistribution {

  def fromData(data: Seq[Int]): DiscreteDistribution = {

    val size = data.size
    val occurrences = data.groupBy(n => n).view.mapValues(_.size).toMap
    val distribution = occurrences.view.mapValues(_.toDouble / size.toDouble).toMap

    new DiscreteDistribution(distribution)
  }
}