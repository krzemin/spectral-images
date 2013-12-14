package evolutionary

import spectral.SpectralImage

trait ClusterDetermination {

  def determineCluster(x: Int, y: Int, image: SpectralImage, individual: EvolutionaryAlgorithm#Individual): Int = {
    var min: Double = Double.MaxValue
    var argMin: Int = 0
    var id: Int = 0
    for (cluster <- individual) {
      if (cluster.nonEmpty) {
        var d = 0.0
        for (dim <- 0 until image.depth) {
          d += ((x: Int) => x * x)(image.pixelAt(x, y, dim) - cluster.get(dim))
        }
        argMin = if (min > d) {
          min = d
          id
        } else {
          argMin
        }

      }
      id += 1
    }
    argMin
  }

}
