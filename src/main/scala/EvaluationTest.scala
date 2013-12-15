import evolutionary._
import spectral.{CroppedSpectralImage, RawMultibandlImageReader}

object EvaluationTest extends App {

  val outputPrefix = "resources/output/evaluation/kmi/classification-"

  val hdfImgFiles = List(
    "L71011026_02620020622_B10",
    "L71011026_02620020622_B20",
    "L71011026_02620020622_B30",
    "L71011026_02620020622_B40",
    "L71011026_02620020622_B50",
    "L72011026_02620020622_B70"
  ) map ("resources/input/L71011026_02620020622/" + _ + ".L1G")
  val (width, height) = (6464, 6000)

  val img = RawMultibandlImageReader.readImage(width, height, hdfImgFiles)
  val imgCropped = new CroppedSpectralImage(img, 800, 600, 2000, 3000)

  imgCropped.saveAsPng(outputPrefix ++ "image.png", (5,4,3))

  def runEvolutionaryClassification(iterations: Int,
                                    maxClusters: Int,
                                    probabilityOfEmptyCluster: Double): Unit = {
    object Params extends EvolutionaryParameters {
      val populationSize: Int = 50
      val maxIterations: Int = iterations
      val crossoverPercentage: Double = 0.4
      val mutationProbability: Double = 0.1
    }

    val classifier = new UnsupervisedSpectralClassifier(
      Params,
      maxClusters,
      probabilityOfEmptyCluster)
      with KMI
      with SelectionOperators.RouletteWheel
      with CrossoverOperators.OnePointCrossover {}

    val classification = classifier.classify(imgCropped)

    val description = s"it${iterations}-pop${Params.populationSize}-cx${Params.crossoverPercentage}-mut${Params.mutationProbability}"
    classification.saveAsPng(outputPrefix ++ description ++ ".png")
  }

  for(n <- 60 to 200 by 20) {
    println(s"running classification with $n iterations")
    runEvolutionaryClassification(n, 2, 0.0)
  }
}
