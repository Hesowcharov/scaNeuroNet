package Neuro.Layer

/**
  * Created by HesowcharovU on 02.04.2016.
  */
class HiddenLayer (neurons: Int, activateFunction: Double => Double,
                   derivativeFunction: Double => Double, prev: Layer) extends Layer(neurons) {
  val synaps = prev.dim
  private val w = Array.fill(neurons, synaps)(math.random * 0.079)
  private var outp: Option[Array[Double]] = None
  private var derOutp: Option[Array[Double]] = None
  private var prevL: Option[Layer] = Option(prev)
  override def prevLayer: Option[Layer] = prevL

  override  def in(xs: Array[Double]): Unit = {
    super.in(xs)
    outp = None
  }

  override def out: Array[Double] = {
    require(inp.isDefined)
    outp match {
      case Some(xs) => xs
      case None => {
        val ns = for { i <- 0 until neurons } yield w(i).zip(inp.get)
        val sumNs = ns.map{ (ar1) => (.0 /: ar1) {(ac, el) => ac + el._1 * el._2} }.toArray
        val outputNs = sumNs.map { activateFunction }
        outp = Option(outputNs)
        out
      }//
    }
  }

  def derivativeOut: Array[Double] = {
    derOutp match {
      case Some(v) => v
      case None => {
        val derOut = out map { derivativeFunction }
        derOutp = Option(derOut)
        derivativeOut
      }
    }
  }

  def synapsesOfPrevNeuron(neuron: Int): Array[Double] = {
    require(neuron < prev.dim)
    val seq = for (i <- 0 until this.dim) yield w(i)(neuron)
    seq.toArray
  }

}

object HiddenLayer {
  private def activateFunc(arg: Double): Double = 1 / (1 + math.pow(math.E, -arg))
  private def derivativeFunc(out: Double): Double = { out * (1 - out) }
  def apply(neurons: Int, prevLayer: Layer): HiddenLayer = {
    new HiddenLayer(neurons, activateFunc, derivativeFunc, prevLayer)
  }
  def redefine(l: HiddenLayer, deltas: Array[Double], speed: Double = 0.666): Unit = {
    require(l.inp.isDefined)
    for (i <- 0 until l.dim) {
      l.w(i) = l.w(i). zip(l.inp.get)
        .map { case (a, b) => a + speed * deltas(i) * b * l.out(i) * l.derivativeOut(i)  }
    }
  }
}
