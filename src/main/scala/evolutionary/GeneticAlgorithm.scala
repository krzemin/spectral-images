package evolutionary

trait GeneticAlgorithm {

  type Individual
  type Population = List[Individual]

  val populationSize: Int
  val maxIterations: Int
  val crossoverPercentage: Double
  val mutationProbability: Double

  val crossover: Population => Population
  val mutation: Population => Population
  val fitness: Individual => Int
  val randomPopulation: Int => Population

  def evolution(): Population = {
    def aux(n: Int, population: Population): Population = n match {
      case 0 => population
      case n =>
        val cxPopulation = crossover(population)
        val mutPopulation = mutation(cxPopulation)
        aux(n-1, mutPopulation)
    }

    aux(maxIterations, randomPopulation(populationSize))
  }

}
