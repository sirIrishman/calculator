package main.scala.calculator

import scala.collection.mutable.{Stack, ListBuffer}

object NotationConverter {
  // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  val precedence = Map(
    "(" -> 1, ")" -> 1,
    "*" -> 3, "/" -> 3, "%" -> 3, "^" -> 3,
    "+" -> 4, "-" -> 4
  )
  val operators = precedence.keys.toList

  def fromInfixToPostfix(infixTokens: List[Token]): Either[List[Token], ExpressionError] = {
    val postfixTokens = ListBuffer[Token]()
    val operatorsStack = Stack[Token]()
    infixTokens.foreach(token => {
      token match {
        case NumberToken(_, _) =>
          postfixTokens += token
        case OperatorToken("(", _) =>
          operatorsStack.push(token)
        case OperatorToken(")", _) => {
          while (!operatorsStack.isEmpty && operatorsStack.head.text != "(") {
            postfixTokens += operatorsStack.pop()
          }
          if (operatorsStack.isEmpty) {
            return Right(new ExpressionError(s"Parentheses are not balanced"))
          }
          operatorsStack.pop()
        }
        case OperatorToken(operator, _) if operators contains operator => {
          while (!operatorsStack.isEmpty && operatorsStack.head.text != "(" && precedence(operator) >= precedence(operatorsStack.head.text)) {
            postfixTokens += operatorsStack.pop()
          }
          operatorsStack.push(token)
        }
        case _ =>
          return Right(new ExpressionError(s"Failed to parse ${token} token"))
      }
    })
    if (operatorsStack.exists(token => token.text == "(")) {
      return Right(new ExpressionError(s"Parentheses are not balanced"))
    }
    postfixTokens ++= operatorsStack.toList
    Left(postfixTokens.toList)
  }
}
