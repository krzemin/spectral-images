package classification

import spectral.SpectralImage
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

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
