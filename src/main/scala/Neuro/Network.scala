
package Neuro

import Neuro.Layer._

import scala.annotation.tailrec

class Network private (firstDim: Int/*, speed*/) {
  private val inLayer: Option[InputLayer] = Option(InputLayer(firstDim))
  private var outLayer: Option[HiddenLayer] = None

  def activate(input: Array[Double]): Array[Double] = {
    require(inLayer.isDefined && outLayer.isDefined)
    require(input.length == inLayer.get.dim)
    @tailrec
    def throwInput(l: Option[Layer], inp: Array[Double]): Array[Double] = {
      l match {
        case Some(nl) => nl.in(inp); throwInput(nl.nextLayer, nl.out)
        case None => inp
      }
    }
    throwInput(inLayer, input)
  }

  def !!(inSample: Array[Double], outSample: Array[Double]): Network = {
    require(inLayer.isDefined && outLayer.isDefined)
    require(inSample.length == inLayer.get.dim)
    require(outSample.length == outLayer.get.dim)

    @tailrec
    def findDeltas(ls: List[(HiddenLayer, Array[Double])]): List[(HiddenLayer, Array[Double])] = {
      val (l, prevDeltas) = ls.head
      l.prevLayer match {
        case Some(prevL: HiddenLayer) => { 
          val prevWeights = for (i <- 0 until prevL.dim) yield l.synapsesOfPrevNeuron(i)
          val prevErrs = prevWeights
            .map { ar => ar.zip(prevDeltas).foldLeft (.0) {(ac, elem) => ac + elem._1 * elem._2} }
          val seqDeltas = prevErrs.zip(prevL.derivativeOut).map { case (x,y) => x * y }
          val newDeltas = seqDeltas.toArray
          findDeltas( (prevL, newDeltas) :: ls)
        }
        case _ => ls
      }
    }
    
    @tailrec
    def changeWeights(ls: List[(HiddenLayer, Array[Double])]): Unit = {
      ls match {
        case (l:HiddenLayer,deltas) :: tail => {
          HiddenLayer.redefine(l, deltas)
          changeWeights(tail)
        }
        case _ =>
      }
    }
    
    val outReal = activate(inSample)
    val finalError = outSample zip outReal map { x => (x._2) * (1 - x._2) * (x._1 - x._2) }
    val outL = this.outLayer.get
    var ls = List[(HiddenLayer, Array[Double])]( (outL, finalError) )
    ls = findDeltas(ls)
    changeWeights(ls)
    this
  }
  
  def <:>(neurons: Int): Network = {
    //require(inLayer.isDefined)
    this.outLayer match {
      case Some(l) => {
        val  newLayer = HiddenLayer(neurons, l)
        l setNextLayer newLayer
        this.outLayer = Option(newLayer) 
        this
      }
      case None => {
        val firstLayer = inLayer.get
        val  newLayer = HiddenLayer(neurons, firstLayer)
        firstLayer setNextLayer newLayer
        this.outLayer = Option(newLayer) 
        this
      }
    }
  }

  def defineError(in: Array[Double], out: Array[Double]) : Double = {
    val outReal = activate(in)
    val errs = 0.5 * outReal.zip(out).map { case (x,y) => math.pow(x-y, 2) }.foldLeft(.0) { _ + _}
    errs
  }
}

object Network {
  def apply(neurons: Int): Network = new Network(neurons)
}

//