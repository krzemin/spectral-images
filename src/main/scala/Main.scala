
import evolutionary._
import spectral._

object Main extends App {

  // usage of raw multiband image reader

  val hdfImgFiles = List(
     "L71002026_02620000703_B10.L1G"
    ,"L71002026_02620000703_B20.L1G"
    ,"L71002026_02620000703_B30.L1G"
    ,"L71002026_02620000703_B40.L1G"
    ,"L71002026_02620000703_B50.L1G"
    ) map ("resources/input/L71002026_02620000703/" + _)
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
  val hdfImgCropped = new CroppedSpectralImage(hdfImg, 1000, 1000, 2000, 1500)
  hdfImgCropped.saveAsPng("resources/output/hdfImgCropped.png", (1,3,4))


  println("classifying...")

  //SimpleTerrainClassifier.classify(hdfImg).saveAsPng("resources/output/hdfImg_classif.png")

  object Params extends EvolutionaryParameters {
    val populationSize: Int = 6 // must be even to proper crossover
    val maxIterations: Int = 5
    val crossoverPercentage: Double = 0.1
    val mutationProbability: Double = 0.05
  }

  object KMIClassifier
    extends UnsupervisedSpectralClassifier(Params, 4, 0.2)
    with KMI
    with SelectionOperators.RouletteWheel
    with CrossoverOperators.OnePointCrossover
  {}


  val t0 = System.nanoTime
  val classification = KMIClassifier.classify(hdfImgCropped)
  val t1 = System.nanoTime

  println(s"classification took ~${(t1 - t0) / 1000000} ms")

  println("saving classification as PNG")
  classification.saveAsPng("resources/output/hdfImgCropped_classif.png")

}
