package random
import evolutionary.Point
import evolutionary.Chromosome
//for now it's mock random util, just to get some types
object Util {
	def randomPoint : Point = {
		new Point(new Array[Double](3))
	}	
	def randomChromosome : Chromosome = {
		new Chromosome()
	}
	def randomPopulation : Array[Chromosome] = {
	    new Array[Chromosome](3);
	} 
}