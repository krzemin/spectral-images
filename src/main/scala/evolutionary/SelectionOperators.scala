package evolutionary

import spectral.SpectralImage

trait SelectionOperator {

  type Population = EvolutionaryAlgorithm#Population
  type Individual = EvolutionaryAlgorithm#Individual

  def selection(population: Population, fitness: (Individual, SpectralImage) => Double): (Population, Population)
}

object SelectionOperators {

  val rouletteWheelSelection = new SelectionOperator {
    def selection(population: Population, fitness: (Individual, SpectralImage) => Double): (Population, Population) = {
      (population, Array())
    }
  }

}


