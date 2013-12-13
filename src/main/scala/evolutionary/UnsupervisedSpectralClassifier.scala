package evolutionary

import classification.{ImageClassification, SpectralImageClassifier}
import spectral.SpectralImage
import scala.util.Random
import java.awt.Color

abstract class UnsupervisedSpectralClassifier extends SpectralImageClassifier {

  val params: EvolutionaryParameters
  val maxK: Int

  private val rand: Random = new Random()

  def classify(image: SpectralImage): ImageClassification = {
    val img = image

    val algorithm = new EvolutionaryAlgorithm {

      type Cluster = Option[Array[Byte]]
      type Individual = Array[Cluster]

      val parameters: EvolutionaryParameters = params

      def randomCluster: Option[Array[Byte]] =
        if (rand.nextBoolean()) None
        else Some((1 to image.depth).map(_ => rand.nextInt(Byte.MaxValue).toByte).toArray)

      def randomIndividual: Individual =
        (1 to maxK).map(_ => randomCluster).toArray

      // TODO: implement some kind of crossover
      def crossover(crossoverPercentage: Double)(population: Population): Population = population

      // TODO: implement some kind of mutation
      def mutation(mutationProbability: Double)(population: Population): Population = population
    }

    // here we run our evolutionary algorithm
    val finalPopulation = algorithm.runEvolution()

    new ImageClassification {
      type ClassificationValue = Int

      val image: SpectralImage = img

      // TODO: determine classification value of (x,y) pixel of image based on finalPopulation
      def determine(x: Int, y: Int): ClassificationValue = 0

      def renderAsRGBInt(value: ClassificationValue): Int =
        value * Color.BLUE.getRGB / maxK

    }
  }
}
