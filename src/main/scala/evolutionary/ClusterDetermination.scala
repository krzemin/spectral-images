package evolutionary

import spectral.SpectralImage

trait ClusterDetermination {

  def determineCluster(x: Int, y: Int, image: SpectralImage, individual: EvolutionaryAlgorithm#Individual): Int = 0

}
