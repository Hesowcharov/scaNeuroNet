import Neuro.Network

object MainApp extends App {
  var net = Network (1) <:> 1
  net = net.!! (Array(10), Array(0))
    .!!(Array(15), Array(0))
    .!!(Array(-2), Array(0))
    .!!(Array(17), Array(1))
    .!!(Array(125), Array(1))
    .!!(Array(56), Array(1))
  val outp = net.activate(Array(125))
  println(outp.mkString(" "))
}