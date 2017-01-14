package main.scala.calculator

object Main extends App {
  var input = scala.io.StdIn.readLine()
  println(s"input:  ${input}")
  val output = new Calculator().Calc(input)
  println(s"output: ${output}")
}
