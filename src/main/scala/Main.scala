
import evolutionary._
import spectral._

object Main extends App {

  // usage of raw multiband image reader

  val hdfImgFiles = List(
     "B10","B20","B30","B40","B50"
    ) map ("resources/input/L71002026_02620000703/L71002026_02620000703_" + _ + ".L1G")
  val (width, height) = (6476, 6000)


//  val hdfImgFiles = List(
//    "L71045025_02520000716_B10.L1G"
//    ,"L71045025_02520000716_B20.L1G"
//    ,"L71045025_02520000716_B30.L1G"
//    ,"L71045025_02520000716_B40.L1G"
//    ,"L71045025_02520000716_B50.L1G"
//  ) map ("resources/input/L71045025_02520000716/" + _)
//  val (width, height) = (6454, 6002)

  println("reading bands...")
  val hdfImg = RawMultibandlImageReader.readImage(width, height, hdfImgFiles)

//  println("writing PNG...")
//  hdfImg.saveAsPng("resources/output/hdfImg.png", (1,3,4))

  println("cropping image...")
  val hdfImgCropped = new CroppedSpectralImage(hdfImg, 400, 400, 2000, 1500)
  hdfImgCropped.saveAsPng("resources/output/hdfImgCropped.png", (1,3,4))


  println("classifying...")

  //SimpleTerrainClassifier.classify(hdfImg).saveAsPng("resources/output/hdfImg_classif.png")
  
  val popSizes = List(30, 60, 100)
  val coPrecentages = List(0.4, 0.6, 0.8)
  val mutProbs = List(0.05, 0.25, 0.5)
  def createEvolutionParams(params: (Int,Double,Double)) = 
                new EvolutionaryParameters {
                    val populationSize: Int = params._1 // must be even to proper crossover
                    val maxIterations: Int = 100
                    val crossoverPercentage: Double = params._2
                    val mutationProbability: Double = params._3
                }
  def createKMIClassifier(params: EvolutionaryParameters) =
                new UnsupervisedSpectralClassifier(params, 6, 0.1)
                    with KMI
                    with SelectionOperators.RouletteWheel
                    with CrossoverOperators.OnePointCrossover
                {}
  val evoParamsList = (for { a <- popSizes;
                             b <- coPrecentages;
                             c <- mutProbs} yield (a, b, c)) map createEvolutionParams
  val KMIClassifiersList = evoParamsList map createKMIClassifier
  
  def runClassify(classifier: UnsupervisedSpectralClassifier) = {
    val t0 = System.nanoTime
    val classification = classifier.classify(hdfImgCropped)
    val t1 = System.nanoTime
    
    val p = classifier.params
    val paramsString = p.populationSize + "-" + p.crossoverPercentage + "-" + p.mutationProbability
    println(s"classification with params " + paramsString + " took ~${(t1 - t0) / 1000000} ms")
    
    println("saving classification as PNG")
    classification.saveAsPng("resources/output/hdfImgCropped_classif-" + paramsString + ".png")
  }
  KMIClassifiersList map runClassify
}
