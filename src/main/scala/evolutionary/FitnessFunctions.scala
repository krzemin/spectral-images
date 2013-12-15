package evolutionary

import spectral.SpectralImage
import scala.math.sqrt

trait KMI extends ClusterDetermination {
  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var ret: Double = 0.0
    for (x <- 0 until image.width;
         y <- 0 until image.height;
         tmp = individual(determineCluster(x, y, image, individual))) {
      var d = 0.0
      for (dim <- 0 until image.depth) {
        val t = image.pixelAt(x, y, dim) - tmp.get(dim)
        d += t * t
      }
      ret += sqrt(d)
    }
    1 / ret
  }
}

trait XBI extends KMI {
  override def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var min: Double = Double.MaxValue
    for (cluster <- individual;
         other <- individual
         if other != cluster && other.isDefined && cluster.isDefined
    ) {
      val d = (0 until image.depth)
        .map(dim => other.get(dim) - cluster.get(dim))
        .map(v => v * v)
        .sum
      min = math.min(min, d)
    }

    super.fitness(image)(individual) * image.height * image.width * min
  }
}

trait DBI {
  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    0
  }
}

