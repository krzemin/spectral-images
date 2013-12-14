package evolutionary

import scala.util.Random

trait CrossoverOperator {

  def crossoverIndividuals(pair: (EvolutionaryAlgorithm#Individual, EvolutionaryAlgorithm#Individual), rand: Random): (EvolutionaryAlgorithm#Individual, EvolutionaryAlgorithm#Individual)
}

object CrossoverOperators {

  trait OnePointCrossover extends CrossoverOperator {
    def crossoverIndividuals(pair: (EvolutionaryAlgorithm#Individual, EvolutionaryAlgorithm#Individual), rand: Random): (EvolutionaryAlgorithm#Individual, EvolutionaryAlgorithm#Individual) = {
      val (ind1, ind2) = pair
      val len = ind1.size
      val r = rand.nextInt(len)
      (ind1.take(r) ++ ind2.drop(r), ind2.take(r) ++ ind1.drop(r))
    }

  }

}
