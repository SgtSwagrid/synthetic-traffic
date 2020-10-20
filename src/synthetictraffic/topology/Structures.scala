package synthetictraffic.topology

object Structures {

  case class Network(
    topology: Topology,
    shortestPaths: Map[Pair, Path],
    linkLoads: Map[Link, Int],
    trafficMatrix: TrafficMatrix
  )

  case class Topology(
    routers: Seq[Router] = Seq(),
    links: Map[Pair, Link] = Map(),
    sources: Map[Router, Source] = Map(),
    sinks: Map[Router, Sink] = Map()
  ) {
    def pairs(): Seq[Pair] = {
      for(router1 <- routers; router2 <- routers)
        yield (router1, router2)
    }
  }

  case class Router(
    name: String,
    city: String = "",
    latitude: String = "0.0",
    longitude: String = "0.0"
  )

  case class Link(
    from: Router,
    to: Router,
    capacity: Int = 0,
    weight: Int = 0
  )

  sealed trait Boundary

  case class Source(
    to: Router,
    capacity: Int = 0
  ) extends Boundary

  case class Sink(
    from: Router,
    capacity: Int = 0
  ) extends Boundary

  case class Path(
    routers: Seq[Router],
    links: Seq[Link],
    cost: Int
  )

  type Pair = (Router, Router)
  type TrafficMatrix = Map[Pair, Int]
}