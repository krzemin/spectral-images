package evolutionary

import scala.collection.mutable.ArraySeq
import scala.util.Random

trait SelectionOperator {

  def selection(population: EvolutionaryAlgorithm#Population, fitness: EvolutionaryAlgorithm#Individual => Double, rand: Random): EvolutionaryAlgorithm#Population
}

object SelectionOperators {

  private def calculateDistribution(items: Array[Double]): Array[Double] = {
    val buffer: ArraySeq[Double] = ArraySeq(items.size)
    buffer(0) = items.head
    for(i <- 1 until items.size) {
      buffer(i) = buffer(i - 1) + items(i)
    }
    buffer.toArray
  }

  trait RouletteWheel extends SelectionOperator {
    def selection(population: EvolutionaryAlgorithm#Population, fitness: EvolutionaryAlgorithm#Individual => Double, rand: Random): EvolutionaryAlgorithm#Population = {
      val grades = population.map(fitness(_))
      val gradesDistribution = calculateDistribution(grades)
      val gradesSum = grades.sum
      def randomIndividual: EvolutionaryAlgorithm#Individual = {
        // TODO: implement it
        val r = rand.nextDouble() * gradesSum
        population.head
      }

      ((1 to population.size) map (_ => randomIndividual)).toArray
    }
  }

}


