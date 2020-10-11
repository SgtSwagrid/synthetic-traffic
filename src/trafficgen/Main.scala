package trafficgen

import trafficgen.importers.TSVParser

object Main extends App {

  println(TSVParser.parseNetwork(io.Source.fromFile("res/AbileneTM-all/topo-2003-04-10.txt").mkString))
}
