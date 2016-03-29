import Neuro._

object MainApp extends App {
  val net = Network (5) <:> 3 <:> 7 <:> 12
  println("It was done!")
}