package synthetictraffic

import synthetictraffic.generators.{GravityGenerator, LogNormalGenerator, OspfRouter}
import synthetictraffic.parsers.TSVParser
import synthetictraffic.statistics.{BenfordDistribution, DiscreteDistribution, KolmogorovSmirnov}

object Main extends App {

  val topology = TSVParser.parseTopology(
    io.Source.fromFile("res/AbileneTM-all/topo-2003-04-10.txt").mkString).get

  val distr = DiscreteDistribution.fromData((1 to 1000).flatMap { seed =>
    new GravityGenerator(new LogNormalGenerator(seed = seed), OspfRouter)
      .generateNetwork(topology)
      .linkLoads.values.map(BenfordDistribution.firstDigit(_, 10))
      .filter(_ != 0).toSeq
  })

  val error = KolmogorovSmirnov.error(distr, BenfordDistribution.withBase(10))
  println(distr)
  println(error)
}