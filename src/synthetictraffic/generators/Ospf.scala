package synthetictraffic.generators

import synthetictraffic.topology.Structures._
import synthetictraffic.generators.Generators.TrafficRouter

object Ospf extends TrafficRouter {

  def shortestPaths(topology: Topology): Map[Pair, Path] = {

    val dist = topology.pairs.map { pair => pair -> (
      if(pair._1 == pair._2) 0 else
      topology.links.get(pair).map(_.weight).getOrElse(1 << 24)
    )}.toMap

    val next = topology.pairs.map { pair => pair -> (
      if(pair._1 == pair._2) Some(pair._2) else
      topology.links.get(pair).map(_.to)
    )}.toMap

    val (_, paths) = topology.routers.foldLeft(dist, next) {
      case ((dist, next), by) =>

        topology.pairs.foldLeft(dist, next) {
          case ((dist, next), (from, to)) =>

            if (dist(from, to) > dist(from, by) + dist(by, to)) (
              dist + ((from, to) -> (dist(from, by) + dist(by, to))),
              next + ((from, to) -> next(from, by))
            ) else (dist, next)
        }
    }

    getPaths(topology, paths.view.mapValues(_.get).toMap)
  }
}