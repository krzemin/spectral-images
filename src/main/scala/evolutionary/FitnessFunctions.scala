package evolutionary

import spectral.SpectralImage
import scala.math.sqrt

trait KMI extends ClusterDetermination {
  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var ret: Double = 0.0;
    for (x <- 0 until image.width) {
      for (y <- 0 until image.height) {
        val tmp = individual(determineCluster(x, y, image, individual));
        var d = 0.0;
        for (dim <- 0 until image.depth) {
          d += (((x: Int) => x * x)(image.pixelAt(x, y, dim) - tmp.get(dim)));
        }
        ret += sqrt(d);
      }
    }
    1 / ret
  }
}

trait XBI extends KMI {
  override def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var min: Double = Double.MaxValue
    var id: Int = 0
    for (cluster <- individual) {
      for (other <- individual) {
        if (other != cluster && other.isDefined && cluster.isDefined) {
          var d = 0.0;
          for (dim <- 0 until image.depth) {
            d += (((x: Int) => x * x)(other.get(dim) - cluster.get(dim)));
          }
          min = if (min > d) {
            d
          } else {
            min
          }
        }
      }
    }
    super.fitness(image)(individual) * image.height * image.width * min
  }
}

trait DBI {
  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    0
  }
}

