import Neuro.{ Network, Teacher }
import scala.annotation.tailrec

object MainApp extends App {
  val initNetwork = Network(3) <:> 3 <:> 4 <:> 3 <:> 1
  val inputFile = args(0)
  val periodicSequence = io.Source.fromFile(inputFile)
    .getLines
    .map { _.split(",") }
    .map { _(1).toDouble }
    .toArray
  val teacher = new Teacher(-0.5, 0.5, -1, 1)
  
  def preparePair(index: Int, data: Array[Double]): (Array[Double], Array[Double]) = {
    val m1 = data(index)
    val m2 = data(index + 1)
    val m3 = data(index + 2)
    val next = data(index + 3)
    (Array(m1, m2, m3).map { teacher.changeScale }, Array(next).map { teacher.changeScale })
  }
  
  def studying(neuroNet: Network, data: Array[Double], accuracy: Double): (Network, Int) = {

    def getAccuracy(net: Network, counter: Int): Double = {

      @tailrec
      def recChecking(accum: Double, counter: Int): Double = {
        counter match {
          case 0 => accum
          case _ => {
            val randInd = math.random.toInt * (data.length - 3)
            val (inControl, outControl) = preparePair(randInd, data)
            val newAccum = accum + net.defineError(inControl, outControl)
            recChecking(newAccum, counter - 1)
          }
        }
      }
      
      recChecking(0, counter)
    }
    
    @tailrec
    def studyForEpoch(net: Network, indexes: List[Int]): Network = {
      indexes match {
        case index :: tail => {
          val teachingPair = preparePair(index, data)
          val teachedNet = teacher.teach(neuroNet, teachingPair)
          studyForEpoch(teachedNet, tail)
        }
        case Nil => net
      }
    }
    
    @tailrec
    def recStudy(net: Network, epoch: Int): (Network, Int) = {
      val realAccuracy = getAccuracy(net, 3)
      if (realAccuracy <= accuracy) {
        (net, epoch)
      } else {
        val shuffledIndexes = util.Random.shuffle(List.range(1, data.length - 3))
        val teachedNet = studyForEpoch(net, shuffledIndexes)
        recStudy(teachedNet, epoch+1)
      }
    }
    
    recStudy(neuroNet, 0)
  }
  
  def generateNext(net: Network, data: List[Double], counter: Int): List[Double] = {
    val m3 :: m2 :: m1 :: _ = data
    val input = Array(m1, m2, m3)
    val output = net.activate(input)
    val nextValue = output(0)
    if (counter > 0) {
      generateNext(net, nextValue::data, counter - 1)
    } else {
      data
    }
  }
  
  def outputPrognosis(outputFile: String, data: List[Double]): Unit = {
    val writer = new java.io.PrintWriter(new java.io.File(outputFile))
    for { value <- data.reverseIterator } {
        writer.println(value)
    }
    writer.close()
  }

  val (trainedNetwork, epoches) = studying(initNetwork, periodicSequence, 0.01)
  val firstSeq = List( 0.099, 0.049, 0).map { teacher.changeScale }
  val prognosisData = generateNext(trainedNetwork, firstSeq, 40).map { teacher.resetScale }
  val outputFile = "prognosis.csv"
  outputPrognosis(outputFile, prognosisData)
  println(s"Epoches for tranning: $epoches")
}