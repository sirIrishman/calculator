package main.scala.calculator

object Main extends App {
  val input: String = scala.io.StdIn.readLine()
  println(s" input: ${input}")
  val output: Either[String, ExpressionError] = new InfixCalculator().calculate(input)
  output match {
    case Left(result) => println(s"output: ${result}")
    case Right(error) => println(s"output: ERROR. ${error.message}")
  }
}
