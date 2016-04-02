package Neuro.Layer

//2 конструктора - входной слой и промежуточный
abstract class Layer(n: Int) {
  def dim: Int = n
  protected var inp: Option[Array[Double]] = None
  private var nextL: Option[Layer] = None
  def prevLayer: Option[Layer] = None
  def nextLayer: Option[Layer] = nextL
  def setNextLayer(newLayer: Layer): Unit = nextL = Option(newLayer)
  def in(xs: Array[Double]): Unit = {
    require(dim == xs.length)
    inp = Option(xs)
    }
  def out: Array[Double]
}