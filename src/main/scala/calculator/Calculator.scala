package main.scala.calculator

class Calculator {
  def Calc(input: String): String = {
    val sanitizedInput = input.filter(ch => !ch.isSpaceChar)
    val infixOperands = sanitizedInput.toList
    val postfixOperands = NotationConverter.FromInfixToPostfix(infixOperands)
    postfixOperands mkString
  }
}
