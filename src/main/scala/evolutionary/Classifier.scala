package evolutionary
import random.Util
class Classifier (crossC: (Chromosome, Chromosome) => (Chromosome, Chromosome), mutateC: Chromosome => Chromosome, distanceC: (Point, Point) => Double){
	//stuff from outside
	def cross = crossC;
	def mutate = mutateC;
	def distance = distanceC;
	var pop : Array[Chromosome] = Util.randomPopulation
	
	
	// compute distance and assign point to closest cluster 
	def computeDisance(chromosome: Chromosome, point : Point) : (Double, Point) = {
	    //not implemented yet, because dont have for now chromosome impl, so we ahve here some mock
	  val w : Double = 1.0;
	  val point : Point = Util.randomPoint
	  (w,point)
	}
	
	//compute fittness for chromosome
	def computeFitness(chromosome : Chromosome, data : Array[Point]): (Double, Array[Point])= {
		def f : Point => (Double, Point)  = this.computeDisance(chromosome, _ );
		//should write this in some functional way, but not now
		var sum : Double = 0.0
		var ret : Array[Point] = new Array[Point](0);
		for((w,x) <- data.map(f)){
		    sum += w;
		    ret :+ x;
		}
		(sum, ret);
	}
	
	def classify(iter: Int, pop : Int, dataP: Array[Point]): Array[Point] = {
		var data = dataP;
		  
	  
	//	  scala.util.Sorting.stableSort(v,
     //| (e1: Tuple2[Char, Int], e2: Tuple2[Char, Int]) => e1._2 < e2._2)
	  
	    data;
	}
  
}