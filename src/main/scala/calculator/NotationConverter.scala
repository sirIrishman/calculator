package main.scala.calculator

import scala.collection.mutable.{Stack, ListBuffer}

object NotationConverter {
  // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  val Precedence = Map(
    "(" -> 1, ")" -> 1,
    "*" -> 3, "/" -> 3,
    "+" -> 4, "-" -> 4
  )
  val Operators = Precedence.keys.toList

  def FromInfixToPostfix(operands: List[Token]): Either[List[Token], ExpressionError] = {
    val result = ListBuffer[Token]()
    val operatorsStack = Stack[Token]()
    operands.foreach(token => {
      token.Type match {
        case TokenType.Number =>
          result += token
        case TokenType.Operator =>
          token.Text match {
            case "(" =>
              operatorsStack.push(token)
            case ")" => {
              while (!operatorsStack.isEmpty && operatorsStack.head.Text != "(") {
                result += operatorsStack.pop()
              }
              if (operatorsStack.isEmpty) {
                return Right(new ExpressionError(s"Parentheses are not balanced"))
              }
              operatorsStack.pop()
            }
            case operator if Operators contains operator => {
              if (!operatorsStack.isEmpty && operatorsStack.head.Text != "(" && Precedence(operator) >= Precedence(operatorsStack.head.Text)) {
                result += operatorsStack.pop()
              }
              operatorsStack.push(token)
            }
            case _ =>
              return Right(new ExpressionError(s"Failed to parse ${token} token"))
          }
        case _ =>
          return Right(new ExpressionError(s"Failed to parse ${token} token"))
      }
    })
    result ++= operatorsStack.toList
    Left(result.toList)
  }
}
