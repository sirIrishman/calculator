package main.scala.calculator

class Calculator {
  def CalcInfix(input: String): Either[String, ExpressionError] = {
    val sanitizedInput = input.filter(ch => !ch.isSpaceChar)

    val infixOperands = sanitizedInput.toList

    val conversionResult = NotationConverter.FromInfixToPostfix(infixOperands)
    val postfixOperands = conversionResult match {
      case Left(x) => x
      case Right(x) => return Right(x)
    }

    PostfixExpressionEvaluator.Evaluate(postfixOperands) match {
      case Left(x) => Left(x.toString)
      case Right(x) => Right(x)
    }
  }
}
