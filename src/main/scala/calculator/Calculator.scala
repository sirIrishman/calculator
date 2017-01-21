package main.scala.calculator

class Calculator {
  def CalcInfix(input: String): Either[String, ExpressionError] = {
    val safeInput = if (input == null) "" else input

    val infixOperandTokens = Tokenizer.Tokenize(safeInput) match {
      case Left(tokens) => tokens
      case Right(error) => return Right(error)
    }
    val postfixOperands = NotationConverter.FromInfixToPostfix(infixOperandTokens) match {
      case Left(tokens) => tokens
      case Right(error) => return Right(error)
    }
    PostfixExpressionEvaluator.Evaluate(postfixOperands) match {
      case Left(result) => Left(result.toString)
      case Right(error) => Right(error)
    }
  }
}
