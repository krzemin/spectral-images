package random
import evolutionary.Point
import evolutionary.Center
import evolutionary.Indywidual
//for now it's mock random util, just to get some types
object Util {
	def randomPoint : Point = {
		new Point(new Array[Double](3))
	}	
	def randomIndywidual : Indywidual = {
		new Indywidual(new Array[Center](3))
	}
	def randomPopulation : Array[Indywidual] = {
	    new Array[Indywidual](3);
	} 
}