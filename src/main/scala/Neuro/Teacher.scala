package Neuro

/**
  * Created by HesowcharovU on 07.04.2016.
  */
class Teacher (min: Double, max: Double, left: Double, right: Double) {
  def changeScale(value: Double) : Double = (value - min) * (right - left) / (max - min) + left
  def resetScale(value: Double): Double = (max - min) * (value - left) / (right - left) + min
  def teach(net: Network, samples: (Array[Double], Array[Double])): Network = {
    val (inSample, outSample) = samples
    val fixInSample = inSample.map { changeScale }
    val fixOutSample = outSample.map { changeScale }
    net !! (fixInSample, fixOutSample)
  }
}