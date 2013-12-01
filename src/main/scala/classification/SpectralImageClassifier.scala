package classification

import spectral.SpectralImage

trait SpectralImageClassifier {
  def classify(image: SpectralImage): ImageClassification
}
