package synthetictraffic.generators

import synthetictraffic.topology.Structures._

object Generators {

  trait TrafficGenerator {

    def generateNetwork(topology: Topology): Network

    protected def trafficMatrix(topology: Topology, load: (Source, Sink) => Int):
        Map[(Router, Router), Int] = {

      (for(
        from <- topology.routers;
        to <- topology.routers if from != to
      ) yield (from, to) -> (for(
        source <- topology.sources.get(from);
        sink <- topology.sinks.get(to)
      ) yield load(source, sink))
        .getOrElse(0)).toMap
    }
  }

  trait BoundaryGenerator {

    def boundaryLoad(boundary: Boundary): Int

    def sourceLoads(topology: Topology): Map[Source, Int] = {
      topology.sources.values.map(source => source -> boundaryLoad(source)).toMap
    }

    def sinkLoads(topology: Topology): Map[Sink, Int] = {
      topology.sinks.values.map(sink => sink -> boundaryLoad(sink)).toMap
    }
  }

  trait LinkGenerator {

    def linkLoad(link: Link): Int

    def linkLoads(topology: Topology): Map[Link, Int] = {
      topology.links.values.map(link => link -> linkLoad(link)).toMap
    }
  }

  trait TrafficRouter {

    def shortestPaths(topology: Topology): Map[Pair, Path]

    def linkLoads(topology: Topology, paths: Map[Pair, Path], traffic: TrafficMatrix):
        Map[Link, Int] = {

      topology.links.values.map { link => link -> {
        paths.filter { case (_, path) => path.links.contains(link) }
          .map { case (pair, _) => traffic(pair) }.sum
      }}.toMap
    }

    protected def getPaths(topology: Topology, next: Map[Pair, Router]):
        Map[Pair, Path] = {

      topology.pairs.map { case (from, to) => (from, to) -> {

        val routers = Iterator.unfold(from) { router =>
          next(router, to) match {
            case `router` => None
            case next => Some(next, next)
          }
        }.toSeq

        val links = routers.sliding(2).flatMap {
          case Seq(from, to) => Some(topology.links(from, to))
          case Seq(_) => None
        }.toSeq

        Path(routers, links, links.map(_.weight).sum)
      }}
    }.toMap
  }
}