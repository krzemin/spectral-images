package spectral

import java.io.FileInputStream

object RawMultibandlImageReader {

  def readImage(w: Int, h: Int, bandFileList: List[String]): SpectralImage = {

    def readBinaryFile(fileName: String): Array[SpectralImage#Illumination] = {
      val is = new FileInputStream(fileName)
      val cnt = is.available
      val bytes = Array.ofDim[SpectralImage#Illumination](cnt)
      is.read(bytes)
      is.close()
      bytes
    }

    val buffer: Array[Array[SpectralImage#Illumination]] =
      bandFileList.map(readBinaryFile).toArray

    new SpectralImage {
      def pixelAt(x: Int, y: Int, band: Spectrum): Illumination =
        buffer(band)(y * w + x)

      def height: Int = h

      def width: Int = w

      def depth: Int = buffer.size
    }
  }

}