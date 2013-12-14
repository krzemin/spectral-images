
import evolutionary._
import spectral._

object Main extends App {

  // usage of raw multiband image reader

//  val hdfImgFiles = List(
//     "L71002026_02620000703_B10.L1G"
//    ,"L71002026_02620000703_B20.L1G"
//    ,"L71002026_02620000703_B30.L1G"
//    ,"L71002026_02620000703_B40.L1G"
//    ,"L71002026_02620000703_B50.L1G"
//    ) map ("resources/input/L71002026_02620000703/" + _)
//  val (width, height) = (6476, 6000)


  val hdfImgFiles = List(
    "L71045025_02520000716_B10.L1G"
    ,"L71045025_02520000716_B20.L1G"
    ,"L71045025_02520000716_B30.L1G"
    ,"L71045025_02520000716_B40.L1G"
    ,"L71045025_02520000716_B50.L1G"
  ) map ("resources/input/L71045025_02520000716/" + _)
  val (width, height) = (6454, 6002)

  println("reading bands...")
  val hdfImg = RawMultibandlImageReader.readImage(width, height, hdfImgFiles)

  println("writing PNG...")
  hdfImg.saveAsPng("resources/output/hdfImg.png", (1,3,4))

  println("cropping image...")
  val hdfImgCropped = new CroppedSpectralImage(hdfImg, 1000, 1000, 2000, 1500)
  hdfImgCropped.saveAsPng("resources/output/hdfImgCropped.png", (1,3,4))


  println("classifying...")

  //SimpleTerrainClassifier.classify(hdfImg).saveAsPng("resources/output/hdfImg_classif.png")

  object Params extends EvolutionaryParameters {
    val populationSize: Int = 20
    val maxIterations: Int = 10
    val crossoverPercentage: Double = 0.15
    val mutationProbability: Double = 0.2
  }

  object KMIClassifier
    extends UnsupervisedSpectralClassifier(Params, 4, 0.3)
    with KMI
    with SelectionOperators.RouletteWheel
    with CrossoverOperators.OnePointCrossover
  {}

  val classification = KMIClassifier.classify(hdfImgCropped)

  println("saving classification as PNG")
  classification.saveAsPng("resources/output/hdfImgCropped_classif.png")

}
