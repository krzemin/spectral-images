package spectral

class CroppedSpectralImage(val origImage: SpectralImage,
                           val newWidth: Int,
                           val newHeight: Int,
                           val offsetX: Int,
                           val offsetY: Int)
  extends SpectralImage {
  require(offsetX >= 0 && offsetX < origImage.width)
  require(offsetY >= 0 && offsetY < origImage.height)
  require(newWidth > 0)
  require(newHeight > 0)
  require(offsetX + newWidth <= origImage.width)
  require(offsetY + newHeight <= origImage.height)

  def width: Int = newWidth

  def height: Int = newHeight

  def depth: Int = origImage.depth

  def pixelAt(x: Int, y: Int, band: Spectrum): Illumination =
    origImage.pixelAt(x + offsetX, y + offsetY, band)
}
