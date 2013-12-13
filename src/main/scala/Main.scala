
import classification._
import evolutionary.{EvolutionaryParameters, UnsupervisedSpectralClassifier}
import evolutionary.{KMI, XBI, DMI}
import spectral._
import java.awt.Color

// simple concrete spectral image
object SimpleImage extends SpectralImage {
  val width: Int = 1000
  val height: Int = 500
  val depth: Int = 3

  def pixelAt(x: Int, y: Int, lambda: Spectrum): Illumination = {
    require(0 <= x && x < width)
    require(0 <= y && y < height)

    lambda match {
      case 0 => (y % Byte.MaxValue).toByte
      case 1 => ((x ^ y) % Byte.MaxValue / 2).toByte
      case 2 => 0.toByte // (x % Byte.MaxValue).toByte
    }
  }
}

// simple (concrete) terrain classifier for spectral images
object SimpleTerrainClassifier extends SpectralImageClassifier {
  def classify(img: SpectralImage): ImageClassification = new ImageClassification {
    type ClassificationValue = Boolean
    val image = img
    def determine(x: Int, y: Int) =
      img.pixelAt(x, y, 1) >= 50 || img.pixelAt(x, y, 0) < 20
    def renderAsRGBInt(value: ClassificationValue): Int = value match {
      case true => Color.GREEN.getRGB
      case _ => Color.BLUE.getRGB
    }
  }
}

object Main extends App {
  // simple image and classifier usage
  SimpleImage.saveAsPng("resources/output/image.png", (0,1,2))
  SimpleTerrainClassifier.classify(SimpleImage).saveAsPng("resources/output/image_classif.png")

  // usage of raw multiband image reader

  val hdfImgFiles = List(
     "L71002026_02620000703_B10.L1G"
    ,"L71002026_02620000703_B20.L1G"
    ,"L71002026_02620000703_B30.L1G"
    ,"L71002026_02620000703_B40.L1G"
    ,"L71002026_02620000703_B50.L1G"
    ) map ("resources/input/L71002026_02620000703/" + _)

  println("reading bands...")
  val hdfImg = RawMultibandlImageReader.readImage(6476, 6000, hdfImgFiles)

  println("writing PNG...")
  hdfImg.saveAsPng("resources/output/hdfImg.png", (3,3,3))

  println("classifying and writing...")

  //SimpleTerrainClassifier.classify(hdfImg).saveAsPng("resources/output/hdfImg_classif.png")

  object Params extends EvolutionaryParameters {
    val populationSize: Int = 30
    val maxIterations: Int = 50
    val crossoverPercentage: Double = 0.1
    val mutationProbability: Double = 0.05
  }

  object KMIClassifier extends UnsupervisedSpectralClassifier(Params, 5) with KMI {}

  val classification = KMIClassifier.classify(hdfImg)
  classification.saveAsPng("resources/output/hdfImg_classif.png")

}
