package evolutionary

import spectral.SpectralImage
import scala.math.sqrt
trait KMI extends ClusterDetermination{
  def fitness(individual: EvolutionaryAlgorithm#Individual, image: SpectralImage): Double = {
    var ret : Double = 0.0;
    for(x <- 0 to image.height){
      for(y <- 0 to image.width){
    	  val tmp = individual( determineCluster(x,y,image,individual));
    	  var d = 0.0;
    	  for(dim <-0 to image.depth){
    	    d += (((x : Int) => x*x)(image.pixelAt(x, y, dim) - tmp.get(dim)));
    	  }
    	  ret += sqrt(d); 
      }
    }
    1/ret
  }
}

trait XBI extends ClusterDetermination{
  def fitness(individual: EvolutionaryAlgorithm#Individual, image: SpectralImage): Double = {
    var min : Double = Double.MaxValue 
    var id : Int = 0
    for(cluster <- individual){
      for(other <- individual){
          if(other != cluster){
			  var d = 0.0;
			  for(dim <-0 to image.depth){
			    d += (((x : Int) => x*x)(other.get(dim) - cluster.get(dim)));
			  }
			  min = if(min > d) {
			    d
			  }else{
			    min
			  }
          }
      }
    }
    //should use commented version 
    //KMI.fitness(individual, image) * image.height * image.width * min
    image.height * image.width * min
  }
}

trait DBI {
  def fitness(individual: EvolutionaryAlgorithm#Individual, image: SpectralImage): Double = {
    0
  }
}

