import evolutionary._
import spectral.{CroppedSpectralImage, RawMultibandlImageReader}



object EvaluationTest extends App {

  val outputPrefix = "resources/output/evaluation-"

  val hdfImgFiles = List(
    "L71045025_02520000716_B10.L1G"
    ,"L71045025_02520000716_B20.L1G"
    ,"L71045025_02520000716_B30.L1G"
    ,"L71045025_02520000716_B40.L1G"
    ,"L71045025_02520000716_B50.L1G"
  ) map ("resources/input/L71045025_02520000716/" + _)
  val (width, height) = (6454, 6002)

  val img = RawMultibandlImageReader.readImage(width, height, hdfImgFiles)
  val imgCropped = new CroppedSpectralImage(img, 1000, 1000, 200, 2500)

  imgCropped.saveAsPng(outputPrefix ++ "image.png", (2,1,0))

  def runEvolutionaryClassification(iterations: Int,
                                    maxClusters: Int,
                                    probabilityOfEmptyCluster: Double): Unit = {
    object Params extends EvolutionaryParameters {
      val populationSize: Int = 20
      val maxIterations: Int = iterations
      val crossoverPercentage: Double = 0.4
      val mutationProbability: Double = 0.05
    }

    val classifier = new UnsupervisedSpectralClassifier(
      Params,
      maxClusters,
      probabilityOfEmptyCluster)
      with KMI
      with SelectionOperators.RouletteWheel
      with CrossoverOperators.OnePointCrossover {}

    val classification = classifier.classify(imgCropped)

    val description = s"-it${iterations}-pop${Params.populationSize}-cx${Params.crossoverPercentage}-mut${Params.mutationProbability}"
    classification.saveAsPng(outputPrefix ++ description ++ ".png")
  }

  for(n <- 5 to 50 by 5) {
    println(s"running classification with $n iterations")
    runEvolutionaryClassification(n, 4, 0.3)
  }
}
