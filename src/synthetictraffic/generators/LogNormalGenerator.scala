package synthetictraffic.generators

import Math._
import scala.util.Random
import synthetictraffic.topology.Structures._
import synthetictraffic.generators.Generators._

class LogNormalGenerator(
  private val mean: Int = 5,
  private val std: Int = 2,
  private val seed: Int = 0
) extends BoundaryGenerator {

  def boundaryLoad(boundary: Boundary): Int = {
    val random = new Random(boundary.hashCode() ^ seed)
    max(0, exp(mean + std * random.nextGaussian()).toInt)
  }
}