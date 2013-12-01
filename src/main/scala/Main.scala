
import java.awt.Color
import java.awt.image.{Raster, BufferedImage}
import java.io.File
import javax.imageio.ImageIO

trait SpectralImage {
  type Spectrum = Int
  type Illumination = Byte

  def width: Int
  def height: Int
  def depth: Int
  def pixelAt(x: Int, y: Int, lambda: Spectrum): Illumination

  // we could pick at most 3 spectra to visualize as RGB image
  def saveAsPng(path: String, s: (Spectrum, Spectrum, Spectrum)): Unit = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for(i <- 0 until width; j <- 0 until height) {
      val rgb = pixelAt(i, j, s._1) << 16 | pixelAt(i, j, s._2) << 8 | pixelAt(i, j, s._3)
      img.setRGB(i, j, rgb)
    }
    ImageIO.write(img, "png", new File(path))
  }
}

abstract class ImageClassification {
  type ClassificationValue
  val image: SpectralImage
  def determine(x: Int, y: Int): ClassificationValue
  def renderAsRGBInt(value: ClassificationValue): Int

  def saveAsPng(path: String) = {
    val img = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_RGB)
    for(i <- 0 until image.width; j <- 0 until image.height) {
      val classificationValue = determine(i, j)
      img.setRGB(i, j, renderAsRGBInt(classificationValue))
    }
    ImageIO.write(img, "png", new File(path))
  }
}

trait SpectralImageClassifier {
  def classify(image: SpectralImage): ImageClassification
}

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
  SimpleImage.saveAsPng("image.png", (0,1,2))
  SimpleTerrainClassifier.classify(SimpleImage).saveAsPng("image_classif.png")
}
