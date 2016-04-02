package Neuro.Layer

/**
  * Created by HesowcharovU on 02.04.2016.
  */
class InputLayer private (n: Int) extends Layer(n) {
  def out: Array[Double] = {
    require(inp.isDefined)
    inp.get
  }
}

object InputLayer {
  def apply (neurons: Int): InputLayer = new InputLayer(neurons)
}