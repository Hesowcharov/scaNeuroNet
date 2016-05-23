package Neuro.Layer

/**
  * Created by HesowcharovU on 02.04.2016.
  */
class HiddenLayer (neurons: Int, activateFunction: Double => Double,
                   derivativeFunction: Double => Double, prev: Layer) extends Layer(neurons) {
  val synaps = prev.dim
  private val w = Array.fill(neurons, synaps)( randWeight(-0.6, 0.6) )
  private var outp: Option[Array[Double]] = None
  private var derOutp: Option[Array[Double]] = None
  private val prevL: Option[Layer] = Option(prev)
  private def randWeight(a: Double, b: Double): Double = a + math.random * (b - a)

  override def prevLayer: Option[Layer] = prevL

  override  def in(xs: Array[Double]): Unit = {
    require(xs.length == prev.dim)
    super.in(xs)
    outp = None
    derOutp = None
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
      }
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
  private def derivativeFunc(out: Double): Double = out * (1 - out)
  private def hypTh(arg: Double): Double = {
    val v1 = math.exp(arg)
    val v2 = math.exp(-arg)
    (v1-v2)/(v1+v2)
  }
  private def derivativeHypTh(out: Double): Double = (1 + out) * (1 - out)
  def apply(neurons: Int, prevLayer: Layer): HiddenLayer = {
    new HiddenLayer(neurons, hypTh, derivativeHypTh, prevLayer)
  }
  def redefine(l: HiddenLayer, deltas: Array[Double], speed: Double = 0.50): Unit = {
    require(l.inp.isDefined)
    val input = l.inp.get
    for (i <- 0 until l.dim) {
      l.w(i) = l.w(i). zip(input)
        .map { case (a, b) => a + speed * deltas(i) * b }
    }
  }
}
