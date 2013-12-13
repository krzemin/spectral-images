package evolutionary

class Point (coordinatesC : Array[Double]){
	//coordinates of point in coordiantes.length dim space
	val coordinates : Array[Double] = coordinatesC;
	//result (if we want to show something like partial result)
	var culuster : Int = 0;
}