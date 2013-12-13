package evolutionary

class Center(isRealC : Boolean, coordinatesC: Array[Double]) extends Point(coordinatesC : Array[Double]){
	//as read in paper, sometimes we want to hav fake centers, to have different 
	//cluster numbers in one population
	val isReal = isRealC
}