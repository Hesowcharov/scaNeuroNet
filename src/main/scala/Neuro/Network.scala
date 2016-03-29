
package Neuro

import scala.annotation.tailrec

class Network private (firstDim: Int/*, speed*/) {
  private var inLayer: Option[InputLayer] = Option(InputLayer(firstDim))
  private var outLayer: Option[Layer] = None
  def activate(input: Array[Double]): Array[Double] = {
    require(inLayer.isDefined && outLayer.isDefined)
    @tailrec
    def throwInput(l: Option[Layer], inp: Array[Double]): Array[Double] = {
      l match {
        case Some(nl) => nl.in(inp); throwInput(nl.nextLayer, nl.out)
        case None => inp
      }
    }
    throwInput(inLayer, input)
  }
  /*def study(inSample: Array[Double], outSample: Array[Double]): Network = {
    def updateWeigths(l: Layer, errs: Array[Double]): Unit = {
      l.prevLayer match {
        case Some(prevL) => {
          val newErrs = errs
        }
        case None => 
      }
    }
    val outReal = activate(inSample)
    val error = outSample zip outReal map { x => x._1 - x._2 }
    
  }*/
  def <:>(neurons: Int): Network = {
    require(inLayer.isDefined)
    outLayer match {
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
}

object Network {
  def apply(neurons: Int): Network = new Network(neurons)
  
}

//