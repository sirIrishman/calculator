package main.scala.calculator

import scala.collection.mutable.Stack

object PostfixExpressionEvaluator {
  val constants = Map(
    "pi" -> Math.PI, "e" -> Math.E
  )

  def evaluate(postfixTokens: List[Token]): Either[Double, ExpressionError] = {
    val operandsStack = Stack[Double]()
    postfixTokens.foreach(token => {
      token match {
        case NumberToken(_, _) =>
          val value = token.text.toDouble
          operandsStack.push(value)
        case VariableToken(_, _) =>
          val value = if (token.text.head == '-') {
            val constant = token.text.tail.toLowerCase
            if (constants.contains(constant)) {
              -constants(constant)
            } else {
              return Right(new ExpressionError(s"Unknown $token variable"))
            }
          }
          else {
            val constant = token.text.toLowerCase
            if (constants.contains(constant)) {
              constants(constant)
            } else {
              return Right(new ExpressionError(s"Unknown $token variable"))
            }
          }
          operandsStack.push(value)
        case OperatorToken(_, _) => {
          if (operandsStack.length < 2) {
            return Right(new ExpressionError(s"Insufficient number of operands for ${token} operator"))
          }
          val right = operandsStack.pop()
          val left = operandsStack.pop()
          val result: Double = token.text match {
            case "+" => left + right
            case "-" => left - right
            case "*" => left * right
            case "/" => left / right
            case "^" => Math.pow(left, right)
            case "%" => left % right
            case _ => return Right(new ExpressionError(s"Failed to parse ${token} token"))
          }
          operandsStack.push(result)
        }
      }
    })
    if (operandsStack.isEmpty) {
      return Left(0.0)
    } else if (operandsStack.length == 1) {
      return Left(operandsStack.pop())
    }
    return Right(new ExpressionError(s"Invalid expression"))
  }
}
