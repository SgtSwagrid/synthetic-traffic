package trafficgen.topology

object Topology {

  case class Network(
    routers: Seq[Router] = Seq(),
    links: Seq[Link] = Seq()
  )

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
}

