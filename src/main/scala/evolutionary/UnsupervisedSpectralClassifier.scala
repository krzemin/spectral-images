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

  def fitness(individual: Individual, image: SpectralImage): Double
  def determineCluster(x: Int, y: Int, image: SpectralImage, individual: Individual): Int
  def crossoverIndividuals(pair: (Individual, Individual)): (Individual, Individual)
  def selection(population: Population, fitness: (Individual, SpectralImage) => Double): (Population, Population)

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

      // TODO: implement some kind of crossover using fitness function
      def crossover(crossoverPercentage: Double)(population: Population): Population = {
        val (selected, rest) = selection(population, fitness)
        val shuffled = rand.shuffle(selected)
        val pairs = shuffled.grouped(2)
        val cxSelected = pairs.map { case pair =>
          val cxPair = crossoverIndividuals(pair(0), pair(1))
          Array(cxPair._1, cxPair._2)
        }
        (cxSelected.flatten ++ rest).toArray
      }
      // TODO: implement some kind of mutation
      def mutation(mutationProbability: Double)(population: Population): Population = population
    }

    // here we run our evolutionary algorithm
    val finalPopulation: EvolutionaryAlgorithm#Population = algorithm.runEvolution()

    new ImageClassification {
      type ClassificationValue = Int

      val image: SpectralImage = img

      // TODO: determine classification value of (x,y) pixel of image based on finalPopulation
      def determine(x: Int, y: Int): ClassificationValue =
        determineCluster(x, y, image, finalPopulation.head)

      def renderAsRGBInt(value: ClassificationValue): Int =
        value * Color.BLUE.getRGB / maxK

    }
  }
}
