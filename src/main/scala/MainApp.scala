import Neuro.{ Network, Teacher }
import scala.annotation.tailrec

object MainApp extends App {
  var net = Network(3) <:> 2 <:> 1
  val data = io.Source.fromFile("D:\\dataN1.csv").getLines.map { _.split(";") }.map { _(1).toDouble }.toArray
  val t: Teacher = new Neuro.Teacher(-0.5, 0.5, -1, 1)
  def preparingPair(index: Int): (Array[Double], Array[Double]) = {
    val m1 = data(index)
    val m2 = data(index + 1)
    val m3 = data(index + 2)
    val next = data(index + 3)
    (Array(m1, m2, m3), Array(next))
  }
  def studying(epoch: Int): Int = {

    def isSuiteAccuracy(times: Int, accuracy: Double): Boolean = {

      @tailrec
      def recChecking(accum: Double, counter: Int): Double = {
        counter match {
          case 0 => accum
          case _ => {
            val randInd = (math.random * (data.length - 3)).toInt
            val (inControl, outControl) = preparingPair(randInd)
            val newAccum = accum + net.defineError(inControl, outControl)
            recChecking(newAccum, counter - 1)
          }
        }
      }
      
      val totalErrors = recChecking(0, times)
      totalErrors <= accuracy
    }
    
    for (_ <- 0 until data.length) {
      val randInd = (math.random * (data.length - 3)).toInt
      val (inSample, outSample) = preparingPair(randInd)
      t.teach(net, (inSample, outSample))
    }
    
    if (!isSuiteAccuracy(15, 0.01))
      studying(epoch + 1)
    else
      epoch
  }
  
  def writePrognosis() : Unit = {
    
    val writer = new java.io.PrintWriter(new java.io.File("D:\\control2.csv"))
    
    def writeNext(data: List[Double], counter: Int): Unit = {
      val m3 :: m2 :: m1 :: _ = data
      val input = Array(m1, m2, m3)
      val output = net.activate(input)
      val nextValue = output(0)
      val scaledValue = t.resetScale(nextValue)
      writer.println(scaledValue)
      if (counter > 0)
        writeNext(nextValue::data, counter - 1)
    }
    
    val firstSeq = List( 0.099, 0.049, 0).map { t.changeScale(_) }
    writeNext(firstSeq, 40)
    writer.close()
  }

  studying(1)
  writePrognosis()
  
}