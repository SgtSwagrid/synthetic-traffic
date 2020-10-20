package synthetictraffic.generators

import synthetictraffic.generators.Generators._
import synthetictraffic.topology.Structures._

class GravityGenerator(
  val boundaries: BoundaryGenerator,
  val trafficRouter: TrafficRouter
) extends TrafficGenerator {

  def generateNetwork(topology: Topology): Network = {

    val sourceLoads = boundaries.sourceLoads(topology)
    val sinkLoads =  boundaries.sinkLoads(topology)

    val volume = sourceLoads.values.sum

    val traffic = trafficMatrix(topology, { (source, sink) =>
      sourceLoads(source) * sinkLoads(sink) / volume
    })

    val paths = trafficRouter.shortestPaths(topology)
    val loads = trafficRouter.linkLoads(topology, paths, traffic)

    Network(topology, paths, loads, traffic)
  }
}