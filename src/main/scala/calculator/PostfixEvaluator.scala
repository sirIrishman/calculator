package main.scala.calculator

import scala.collection.mutable.Stack

object PostfixEvaluator {
  def Evaluate(operands: List[Char]): Either[Int, ParsingError] = {
    val operandsStack = Stack[Char]()
    operands.foreach(token => {
      token match {
        case number if number isDigit => operandsStack.push(number)
        case operator => {
          if (operandsStack.length < 2) {
            return Right(new ParsingError(s"Insufficient number of operands for '${operator}' operator"))
          }
          val right = operandsStack.pop().asDigit
          val left = operandsStack.pop().asDigit
          val result: Int = operator match {
            case '+' => left + right
            case '-' => left - right
            case '*' => left * right
            case '/' => left / right
            case _ => return Right(new ParsingError(s"Failed to parse '${token}' token"))
          }
          operandsStack.push(result.toString().head)
        }
      }
    })
    if (operandsStack.length != 1) {
      return Right(new ParsingError(s"Invalid expression"))
    }
    Left(operandsStack.pop().asDigit)
  }
}
