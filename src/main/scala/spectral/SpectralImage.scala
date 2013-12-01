package spectral

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File


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
