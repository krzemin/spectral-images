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

    val buffer: Map[Int, Array[SpectralImage#Illumination]] = bandFileList.zipWithIndex.map {
      case (bandFile, idx) => idx -> readBinaryFile(bandFile)
    }.toMap

    new SpectralImage {
      def pixelAt(x: Int, y: Int, lambda: Spectrum): Illumination =
        buffer(lambda)(y * w + x)

      def height: Int = h

      def width: Int = w

      def depth: Int = buffer.size
    }
  }

}