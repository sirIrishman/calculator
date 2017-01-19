package main.scala.calculator

class Calculator {
  def CalcInfix(input: String): Either[String, ExpressionError] = {
    val safeInput = if (input == null) "" else input

    val infixOperandTokens = Tokenizer.Tokenize(safeInput) match {
      case Left(x) => x
      case Right(x) => return Right(x)
    }
    val postfixOperands = NotationConverter.FromInfixToPostfix(infixOperandTokens) match {
      case Left(x) => x
      case Right(x) => return Right(x)
    }
    PostfixExpressionEvaluator.Evaluate(postfixOperands) match {
      case Left(x) => Left(x.toString)
      case Right(x) => Right(x)
    }
  }
}
