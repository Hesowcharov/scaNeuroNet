
package Neuro

import Neuro.Layer._

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

  def !!(inSample: Array[Double], outSample: Array[Double]): Network = {
    require(inLayer.isDefined && outLayer.isDefined)
    
    @tailrec
    def findDeltas(ls: List[(Layer, Array[Double])]): List[(Layer, Array[Double])] = {
      val (l, errs) = ls.head
      l.prevLayer match {
        case Some(prevL: HiddenLayer) => { 
          val prevWeights = for (i <- 0 until prevL.dim) yield prevL.synapsesOfPrevNeuron(i)
          val seqErrs = prevWeights
            .map { ar => ar.zip(errs).foldLeft (.0) {(ac, elem) => ac + elem._1 * elem._2} }
          val newErrs = seqErrs.toArray
          findDeltas( (prevL, newErrs) :: ls)
        }
        case _ => ls
      }
    }
    
    @tailrec
    def changeWeights(ls: List[(Layer, Array[Double])]): Unit = {
      ls match {
        case (l:HiddenLayer,deltas) :: tail => {
          HiddenLayer.redefine(l, deltas)
          changeWeights(tail)
        }
        case _ =>
      }
    }
    
    val outReal = activate(inSample)
    val finalError = outSample zip outReal map { x => x._1 - x._2 }
    val outL = this.outLayer.get
    var ls = List[(Layer, Array[Double])]( (outL, finalError) )
    ls = findDeltas(ls)
    changeWeights(ls)
    this
  }
  
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