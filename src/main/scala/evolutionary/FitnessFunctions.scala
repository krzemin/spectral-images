package evolutionary

import spectral.SpectralImage
import scala.math.sqrt
import scala.collection.mutable.ArrayBuffer

trait KMI extends ClusterDetermination {
  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var ret: Double = 0.0
    for (x <- 0 until image.width;
         y <- 0 until image.height;
         cluster = individual(determineCluster(x, y, image, individual))) {
      if (cluster.isEmpty) return 0.0
      var d = 0.0
      for (dim <- 0 until image.depth) {
        val t = image.pixelAt(x, y, dim) - cluster.get(dim)
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

trait DBI extends ClusterDetermination {
  def makeClusters(image: SpectralImage, individual: EvolutionaryAlgorithm#Individual): Array[Array[(Int, Int)]] = {
    var ret = Array.fill[ArrayBuffer[(Int, Int)]](individual.length)(new ArrayBuffer[(Int, Int)](0))
    for (x <- 0 until image.width;
         y <- 0 until image.height) {
      ret(determineCluster(x, y, image, individual)).append((x, y))
    }
    ret.map(_.toArray)
  }

  def getPointAsArray(x: Int, y: Int, image: SpectralImage): Array[Byte] = {
    var ret = new Array[Byte](image.depth)
    for (i <- 0 until image.depth) {
      ret(i) = image.pixelAt(x, y, i)
    }
    ret
  }

  def makeMiddle(points: Array[(Int, Int)], image: SpectralImage): Array[Double] = {
    var ret = Array.fill[Double](image.depth)(0.0)
    if (points.length == 0) {
      return ret
    }

    for ((x, y) <- points) {
      ret = (ret, getPointAsArray(x, y, image)).zipped.map((_ + _))
    }
    ret.map(_ / points.length)
  }

  def fitness(image: SpectralImage)(individual: EvolutionaryAlgorithm#Individual): Double = {
    var s: ArrayBuffer[Double] = new ArrayBuffer[Double](0);
    var ret: Double = 0.0
    var middles: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0);
    for (cluster <- makeClusters(image, individual.flatten.map((x => Some(x))))) {
      val middle = makeMiddle(cluster, image);
      middles += middle;
      var clusterWeight = 0.0
      for ((x, y) <- cluster) {
        var d = 0.0
        for (dim <- 0 until image.depth) {
          d += ((x: Double) => x * x)(image.pixelAt(x, y, dim) - middle(dim))
        }
        clusterWeight += d;
      }
      s += sqrt(clusterWeight / cluster.length)
    }
    for ((mid, w) <- middles.zip(s)) {
      var max = 0.0;
      for ((other, ow) <- middles.zip(s)) {
        if (other != mid) {
          var d = 0.0
          for (dim <- 0 until image.depth) {
            val t = mid(dim) - other(dim)
            d += t * t
          }
          if (d > 0)
            d = (w + ow) / sqrt(d)

          max = math.max(max, d)
        }
      }
      ret += max;
    }
    individual.flatten.length / ret;
  }

}
