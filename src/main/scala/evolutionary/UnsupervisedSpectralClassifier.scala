package evolutionary

import classification.{ImageClassification, SpectralImageClassifier}
import spectral.SpectralImage
import scala.util.Random
import java.awt.Color

abstract class UnsupervisedSpectralClassifier(
    val params: EvolutionaryParameters,
    val maxK: Int)
  extends SpectralImageClassifier {
  require(maxK >= 2)

  type Individual = EvolutionaryAlgorithm#Individual
  type Population = EvolutionaryAlgorithm#Population

  def fitness(image: SpectralImage)(individual: Individual): Double
  def determineCluster(x: Int, y: Int, image: SpectralImage, individual: Individual): Int
  def selection(population: Population, fitness: Individual => Double, rand: Random): Population

  // TODO: abstract it
  def crossoverIndividuals(pair: (Individual, Individual)): (Individual, Individual) = pair

  private val rand: Random = new Random()

  def classify(image: SpectralImage): ImageClassification = {
    val img = image

    val algorithm = new EvolutionaryAlgorithm {

      val parameters: EvolutionaryParameters = params

      def randomCluster: Cluster =
        if (rand.nextBoolean()) None
        else Some((1 to image.depth).map(_ => rand.nextInt(Byte.MaxValue).toByte).toArray)

      def randomIndividual: Individual =
        (1 to maxK).map(_ => randomCluster).toArray

      def crossover(crossoverPercentage: Double)(population: Population): Population = {
        val selected: Population = selection(population, fitness(image), rand)
        val shuffled: Population = rand.shuffle(selected.toSeq).toArray
        val pairs = shuffled.grouped(2)
        val cxSelected = pairs.map { case pair =>
          if (rand.nextDouble() <= crossoverPercentage ) {
            val cxPair = crossoverIndividuals(pair(0), pair(1))
            Array(cxPair._1, cxPair._2)
          } else {
            pair
          }
        }
        cxSelected.flatten.toArray
      }

      // TODO: implement some kind of mutation
      def mutation(mutationProbability: Double)(population: Population): Population = population
    }

    // here we run our evolutionary algorithm
    val finalPopulation: EvolutionaryAlgorithm#Population = algorithm.runEvolution()

    // TODO: select best individual according to fitness function
    val bestIndividual = finalPopulation.head

    new ImageClassification {
      type ClassificationValue = Int

      val image: SpectralImage = img

      def determine(x: Int, y: Int): ClassificationValue =
        determineCluster(x, y, image, bestIndividual)

      def renderAsRGBInt(value: ClassificationValue): Int =
        value * Color.BLUE.getRGB / maxK

    }
  }
}
