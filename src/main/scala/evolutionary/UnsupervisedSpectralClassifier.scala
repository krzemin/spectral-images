package evolutionary

import classification.{ImageClassification, SpectralImageClassifier}
import spectral.SpectralImage
import scala.util.Random
import java.awt.Color

import scala.collection.parallel._

abstract class UnsupervisedSpectralClassifier(
    val params: EvolutionaryParameters,
    val maxK: Int,
    val emptyClusterProbability: Double)
  extends SpectralImageClassifier {
  require(maxK >= 2)

  type Individual = EvolutionaryAlgorithm#Individual
  type Population = EvolutionaryAlgorithm#Population

  def fitness(image: SpectralImage)(individual: Individual): Double
  def determineCluster(x: Int, y: Int, image: SpectralImage, individual: Individual): Int
  def selection(population: Population, fitness: Individual => Double, rand: Random): Population
  def crossoverIndividuals(pair: (Individual, Individual), rand: Random): (Individual, Individual)

  private val rand: Random = new Random()

  def classify(image: SpectralImage): ImageClassification = {
    val img = image

    val algorithm = new EvolutionaryAlgorithm {

      val parameters: EvolutionaryParameters = params

      def randomCluster: Cluster =
        if (rand.nextDouble() < emptyClusterProbability) None
        else Some((1 to image.depth).map(_ => rand.nextInt(Byte.MaxValue).toByte).toArray)

      def randomIndividual: Individual =
        (1 to maxK).map(_ => randomCluster).toArray

      def crossover(crossoverPercentage: Double)(population: Population): Population = {
        val selected: Population = selection(population, fitness(image), rand)
        val shuffled: Population = rand.shuffle(selected.toSeq).toArray
        val pairs = shuffled.grouped(2)
        val cxSelected = pairs.map { case pair =>
          if (rand.nextDouble() <= crossoverPercentage ) {
            val cxPair = crossoverIndividuals((pair(0), pair(1)), rand)
            Array(cxPair._1, cxPair._2)
          } else {
            pair
          }
        }
        cxSelected.flatten.toArray
      }

      def mutation(mutationProbability: Double)(population: Population): Population = {
        def mutate(individual: Individual): Individual = {
          val r = rand.nextInt(individual.size)
          individual.updated(r, randomCluster)
        }

        population.map(individual =>
          if (rand.nextDouble() < mutationProbability) individual
          else mutate(individual)
        )
      }
    }

    // here we run our evolutionary algorithm
    val finalPopulation: EvolutionaryAlgorithm#Population = algorithm.runEvolution()

    val bestIndividual = finalPopulation.maxBy(fitness(image))

    val classes = bestIndividual.count(_.isDefined)
    println(s"total number of classes: $classes")

    new ImageClassification {
      type ClassificationValue = Int

      val image: SpectralImage = img

      def determine(x: Int, y: Int): ClassificationValue =
        determineCluster(x, y, image, bestIndividual)

      def renderAsRGBInt(value: ClassificationValue): Int = value match {
        case 0 => Color.GREEN.getRGB
        case 1 => Color.YELLOW.getRGB
        case 2 => Color.BLUE.getRGB
        case 3 => Color.PINK.getRGB
        case 4 => Color.GRAY.getRGB
        case 5 => Color.ORANGE.getRGB
        case 6 => Color.MAGENTA.getRGB
        case 7 => Color.BLACK.getRGB
        case v => v * Color.RED.getRGB / maxK
      }
    }
  }
}
