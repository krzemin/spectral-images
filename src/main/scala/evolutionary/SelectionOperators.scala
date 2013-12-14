package evolutionary

import scala.collection.mutable.ArraySeq
import scala.util.Random
import scala.collection.mutable
import scala.collection.parallel
import scala.collection.parallel.mutable.ParArray

trait SelectionOperator {

  def selection(population: EvolutionaryAlgorithm#Population, fitness: EvolutionaryAlgorithm#Individual => Double, rand: Random): EvolutionaryAlgorithm#Population
}

object SelectionOperators {

  private def calculateDistribution(items: Array[Double]): Array[Double] = {
    val buffer: ArraySeq[Double] = mutable.ArraySeq.fill(items.size)(0)
    buffer(0) = items.head
    for(i <- 1 until items.size) {
      buffer(i) = buffer(i - 1) + items(i)
    }
    buffer.toArray
  }

  trait RouletteWheel extends SelectionOperator {
    def selection(population: EvolutionaryAlgorithm#Population, fitness: EvolutionaryAlgorithm#Individual => Double, rand: Random): EvolutionaryAlgorithm#Population = {
      val populationPar = ParArray.fromTraversables(population)
      val grades: Array[Double] = populationPar.map(fitness).toArray

      val gradesDistribution: Array[Double] = calculateDistribution(grades)
      def randomIndividual: EvolutionaryAlgorithm#Individual = {
        // TODO: improve performance with binary search algorithm
        val r = rand.nextDouble() * gradesDistribution.last

//        val idx = java.util.Arrays.binarySearch(
//          gradesDistribution, 0, grades.size - 1, new java.util.Comparator[Double] {
//            def compare(p1: Double, p2: Double): Int = p1.compareTo(r)
//          })

        var idx = 0
        while (gradesDistribution(idx) < r) { idx += 1 }
        population(idx)
      }

      ((1 to population.size) map (_ => randomIndividual)).toArray
    }
  }

}


