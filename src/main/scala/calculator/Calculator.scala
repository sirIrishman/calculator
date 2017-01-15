package main.scala.calculator

class Calculator {
  def Calc(input: String): Either[String, ParsingError] = {
    val sanitizedInput = input.filter(ch => !ch.isSpaceChar)
    val infixOperands = sanitizedInput.toList
    val conversionResult = NotationConverter.FromInfixToPostfix(infixOperands)
    val postfixOperands = conversionResult match {
      case Left(x) => x
      case Right(x) => return Right(x)
    }
    Left(postfixOperands mkString)
  }
}
