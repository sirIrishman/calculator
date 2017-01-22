package main.scala.calculator

import scala.collection.mutable.{Stack, ListBuffer}

object NotationConverter {
  // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  val precedence = Map(
    "(" -> 1, ")" -> 1,
    "*" -> 3, "/" -> 3,
    "+" -> 4, "-" -> 4
  )
  val operators = precedence.keys.toList

  def fromInfixToPostfix(infixTokens: List[Token]): Either[List[Token], ExpressionError] = {
    val postfixTokens = ListBuffer[Token]()
    val operatorsStack = Stack[Token]()
    infixTokens.foreach(token => {
      token.kind match {
        case TokenKind.Number =>
          postfixTokens += token
        case TokenKind.Operator =>
          token.text match {
            case "(" =>
              operatorsStack.push(token)
            case ")" => {
              while (!operatorsStack.isEmpty && operatorsStack.head.text != "(") {
                postfixTokens += operatorsStack.pop()
              }
              if (operatorsStack.isEmpty) {
                return Right(new ExpressionError(s"Parentheses are not balanced"))
              }
              operatorsStack.pop()
            }
            case operator if operators contains operator => {
              if (!operatorsStack.isEmpty && operatorsStack.head.text != "(" && precedence(operator) >= precedence(operatorsStack.head.text)) {
                postfixTokens += operatorsStack.pop()
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
    if (operatorsStack.exists(token => token.text == "(")) {
      return Right(new ExpressionError(s"Parentheses are not balanced"))
    }
    postfixTokens ++= operatorsStack.toList
    Left(postfixTokens.toList)
  }
}
