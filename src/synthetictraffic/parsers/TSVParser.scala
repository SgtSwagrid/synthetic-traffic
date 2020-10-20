package synthetictraffic.parsers

import synthetictraffic.topology.Structures._

object TSVParser extends NetworkParser {

  def parseTopology(source: String): Option[Topology] = {

    for(
      (routers, links) <- partition(getLines(source));
      routers <- parseRouters(routers);
      links <- parseLinks(links, routers)
    ) yield Topology(routers.values.toSeq, links,
      routers.values.map(router => router -> Source(router)).toMap,
      routers.values.map(router => router -> Sink(router)).toMap)
  }

  private def getLines(source: String): Seq[String] = {

    source.split("\n").toList
      .map(_.trim).filter(!_.isEmpty).filter(!_.startsWith("#"))
  }

  private def partition(lines: Seq[String]): Option[(Seq[String], Seq[String])] = {

    (lines.takeWhile(_ != "link"), lines.dropWhile(_ != "link")) match {
      case ("router" :: routers, "link" :: links) => Some(routers, links)
      case _ => None
    }
  }

  private def parseRouter(line: String): Option[Router] = {

    line.split("\\s").toList match {
      case Seq(name, city, latitude, longitude)
        => Some(Router(name, city, longitude, latitude))
      case _ => None
    }
  }

  private def parseRouters(lines: Seq[String]): Option[Map[String, Router]] = {

    lines.foldLeft[Option[Map[String, Router]]](Some(Map())) {
      case (routers, line) =>
        for(routers <- routers; router <- parseRouter(line))
          yield routers + (router.name -> router)
    }
  }

  private def parseLink(line: String, routers: Map[String, Router]):
      Option[Link] = {

    line.split("\\s").toList match {
      case Seq(from, to, capacity, weight) => for(
        from <- routers.get(from); to <- routers.get(to);
        capacity <- capacity.toIntOption; weight <- weight.toIntOption
      ) yield Link(from, to, capacity, weight)
      case _ => None
    }
  }

  private def parseLinks(lines: Seq[String], routers: Map[String, Router]):
      Option[Map[(Router, Router), Link]] = {

    lines.foldLeft [Option[Map[(Router, Router), Link]]] (Some(Map())) {
      case (links, line) =>
        for(links <- links; link <- parseLink(line, routers))
          yield links + ((link.from, link.to) -> link)
    }
  }
}