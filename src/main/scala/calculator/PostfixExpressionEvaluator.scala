package main.scala.calculator

import scala.collection.mutable.Stack

object PostfixExpressionEvaluator {
  def evaluate(postfixTokens: List[Token]): Either[Double, ExpressionError] = {
    val operandsStack = Stack[String]()
    postfixTokens.foreach(token => {
      token match {
        case NumberToken(_, _) =>
          operandsStack.push(token.text)
        case OperatorToken(_, _) => {
          if (operandsStack.length < 2) {
            return Right(new ExpressionError(s"Insufficient number of operands for ${token} operator"))
          }
          val right = operandsStack.pop().toDouble
          val left = operandsStack.pop().toDouble
          val result: Double = token.text match {
            case "+" => left + right
            case "-" => left - right
            case "*" => left * right
            case "/" => left / right
            case "^" => Math.pow(left, right)
            case "%" => left % right
            case _ => return Right(new ExpressionError(s"Failed to parse ${token} token"))
          }
          operandsStack.push(result.toString())
        }
      }
    })
    if (operandsStack.isEmpty) {
      return Left(0.0)
    } else if (operandsStack.length == 1) {
      return Left(operandsStack.pop().toDouble)
    }
    return Right(new ExpressionError(s"Invalid expression"))
  }
}
