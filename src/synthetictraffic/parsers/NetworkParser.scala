package synthetictraffic.parsers

import synthetictraffic.topology.Structures._

trait NetworkParser {

  def parseTopology(source: String): Option[Topology]
}