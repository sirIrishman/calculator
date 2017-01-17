package main.scala.calculator

import scala.collection.mutable.Stack

object PostfixExpressionEvaluator {
  def Evaluate(operands: List[Char]): Either[Int, ExpressionError] = {
    val operandsStack = Stack[Char]()
    operands.foreach(token => {
      token match {
        case number if number isDigit => operandsStack.push(number)
        case operator => {
          if (operandsStack.length < 2) {
            return Right(new ExpressionError(s"Insufficient number of operands for '${operator}' operator"))
          }
          val right = operandsStack.pop().asDigit
          val left = operandsStack.pop().asDigit
          val result: Int = operator match {
            case '+' => left + right
            case '-' => left - right
            case '*' => left * right
            case '/' => left / right
            case _ => return Right(new ExpressionError(s"Failed to parse '${token}' token"))
          }
          operandsStack.push(result.toString().head)
        }
      }
    })
    if (operandsStack.length != 1) {
      return Right(new ExpressionError(s"Invalid expression"))
    }
    Left(operandsStack.pop().asDigit)
  }
}
