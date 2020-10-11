package trafficgen.importers

import trafficgen.topology.Topology._

trait NetworkParser {
  def parseNetwork(source: String): Option[Network]
}