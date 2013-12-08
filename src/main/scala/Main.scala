
import classification._
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
  SimpleImage.saveAsPng("resources/output/image.png", (0,1,2))
  SimpleTerrainClassifier.classify(SimpleImage).saveAsPng("resources/output/image_classif.png")
}
