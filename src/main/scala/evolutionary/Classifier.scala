package evolutionary
import random.Util
import scala.util.Sorting
class Classifier (crossC: (Indywidual, Indywidual) => (Indywidual, Indywidual), mutateC: Indywidual => Indywidual, fitnessC: (Indywidual, Array[Point]) => Double){
	//stuff from outside
	def cross = crossC;
	def mutate = mutateC;
	def fitness = fitnessC;
	var pop : Array[Indywidual] = Util.randomPopulation
	
	// compute distance and assign point to closest cluster
	// TODO move in some other place, where we will define fitness functions
	//it's not general enough to put it here, but guess i can use it in k-mean idx
	def computeDisance(chromosome: Indywidual, point : Point) : (Double, Point) = {
	    //not implemented yet, because dont have for now chromosome impl, so we ahve here some mock
	  val w : Double = 1.0;
	  val point : Point = Util.randomPoint
	  (w,point)
	}
	
	//compute fittness for individual
	//TODO move in some other place, where we will define fitness functions
	//it's not general enough to put it here, but guess i can use it in k-mean idx
	def computeFitness(indywidual : Indywidual, data : Array[Point]): (Double, Array[Point])= {
		def f : Point => (Double, Point)  = this.computeDisance(indywidual, _ );
		//should write this in some functional way, but not now
		var sum : Double = 0.0
		var ret : Array[Point] = new Array[Point](0);
		for((w,x) <- data.map(f)){
		    sum += w;
		    ret :+ x;
		}
		(sum, ret);
	}
	
	def classify(iter: Int, popCap : Int, bestPop: Int, dataP: Array[Point]): Array[Point] = {
	  //put it in for loop
		var data = dataP;
		def f : Indywidual => (Double, Indywidual)  = (x :Indywidual) => (fitness( x, data), x)
		var list = scala.util.Sorting.stableSort(pop.map(f), (e1: Tuple2[Double, Indywidual], e2: Tuple2[Double, Indywidual]) => e1._1 < e2._1)
		
		//limit pop for reproduction
		
		//cross
		
		//add new list to old-limited list
		
		//mutations
		
	    data;
	}
  
}