package evolutionary

import scala.annotation.tailrec

trait EvolutionaryParameters {
  val populationSize: Int
  val maxIterations: Int
  val crossoverPercentage: Double
  val mutationProbability: Double
}

abstract class EvolutionaryAlgorithm {

  type Individual
  type Population = List[Individual]

  val parameters: EvolutionaryParameters
  val crossover: Double => Population => Population
  val mutation: Double => Population => Population
  val randomIndividual: Individual

  private def randomPopulation: Population =
    ((1 to parameters.populationSize) map { case _ => randomIndividual }) toList

  def runEvolution(): Population = {

    val crossoverOp = crossover(parameters.crossoverPercentage)
    val mutationOp = mutation(parameters.mutationProbability)

    @tailrec
    def evolutionAux(n: Int, population: Population): Population = n match {
      case 0 => population
      case n =>
        val cxPopulation = crossoverOp(population)
        val mutPopulation = mutationOp(cxPopulation)
        evolutionAux(n-1, mutPopulation)
    }

    evolutionAux(parameters.maxIterations, randomPopulation)
  }

}
