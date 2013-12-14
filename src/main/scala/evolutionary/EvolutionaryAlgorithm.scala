package evolutionary

import scala.annotation.tailrec

trait EvolutionaryParameters {
  val populationSize: Int
  val maxIterations: Int
  val crossoverPercentage: Double
  val mutationProbability: Double
}

abstract class EvolutionaryAlgorithm {

  type Cluster = Option[Array[Byte]]
  type Individual = Array[Cluster]
  type Population = Array[Individual]

  val parameters: EvolutionaryParameters
  def crossover(crossoverPercentage: Double)(population: Population): Population
  def mutation(mutationProbability: Double)(population: Population): Population
  def randomIndividual: Individual

  private def randomPopulation: Population =
    (1 to parameters.populationSize).map(_ => randomIndividual).toArray

  def runEvolution(): Population = {

    val crossoverOp: Population => Population = crossover(parameters.crossoverPercentage)
    val mutationOp: Population => Population = mutation(parameters.mutationProbability)

    @tailrec
    def evolutionAux(n: Int, population: Population): Population = n match {
      case 0 =>
        println("evolution finished")
        population
      case _ =>
        println(s"iteration number $n")
        val cxPopulation = crossoverOp(population)
        val mutPopulation = mutationOp(cxPopulation)
        evolutionAux(n - 1, mutPopulation)
    }

    evolutionAux(parameters.maxIterations, randomPopulation)
  }

}
