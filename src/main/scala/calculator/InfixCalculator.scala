package main.scala.calculator

class InfixCalculator {
  def calculate(infixExpression: String): Either[String, ExpressionError] = {
    val safeInfixExpression = if (infixExpression == null) "" else infixExpression

    val infixTokens = InfixTokenizer.tokenize(safeInfixExpression) match {
      case Left(tokens) => tokens
      case Right(error) => return Right(error)
    }
    val postfixTokens = NotationConverter.fromInfixToPostfix(infixTokens) match {
      case Left(tokens) => tokens
      case Right(error) => return Right(error)
    }
    PostfixExpressionEvaluator.evaluate(postfixTokens) match {
      case Left(result) => Left(result.toString)
      case Right(error) => Right(error)
    }
  }
}
