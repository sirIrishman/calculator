package main.scala.calculator

import scala.collection.mutable.Stack

object PostfixExpressionEvaluator {
  def Evaluate(operands: List[Token]): Either[Double, ExpressionError] = {
    val operandsStack = Stack[String]()
    operands.foreach(token => {
      token.Type match {
        case TokenType.Number => operandsStack.push(token.Text)
        case TokenType.Operator => {
          if (operandsStack.length < 2) {
            return Right(new ExpressionError(s"Insufficient number of operands for ${token} operator"))
          }
          val right = operandsStack.pop().toDouble
          val left = operandsStack.pop().toDouble
          val result: Double = token.Text match {
            case "+" => left + right
            case "-" => left - right
            case "*" => left * right
            case "/" => left / right
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
