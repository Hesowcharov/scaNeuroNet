package Neuro

//2 конструктора - входной слой и промежуточный
abstract class Layer(n: Int) {
  def dim: Int = n
  var inp: Option[Array[Double]] = None
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

class InputLayer private (n: Int) extends Layer(n) {
  def out: Array[Double] = {
    require(inp.isDefined)
    inp.get
  }
}

object InputLayer {
  def apply (neurons: Int): InputLayer = new InputLayer(neurons)
}

class HiddenLayer (neurons: Int, activateFunction: Double => Double,
    derivativeFunction: Double => Double, prev: Layer) extends Layer(neurons) {
  val synaps = prev.dim
  private val w = Array.fill(neurons, synaps)(math.random * 0.5)
  private var outp: Option[Array[Double]] = None
  private var derOutp: Option[Array[Double]] = None
  private var prevL: Option[Layer] = Option(prev)
  override def prevLayer: Option[Layer] = prevL
  
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
