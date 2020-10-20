package synthetictraffic

import synthetictraffic.generators.{GravityGenerator, LogNormalGenerator, Ospf}
import synthetictraffic.parsers.TSVParser

object Main extends App {

  val src = io.Source.fromFile("res/AbileneTM-all/topo-2003-04-10.txt").mkString
  val topology = TSVParser.parseTopology(src).get
  val generator = new GravityGenerator(new LogNormalGenerator(), Ospf)
  val network = generator.generateNetwork(topology)
  println(network.linkLoads.mkString("\n"))
}
