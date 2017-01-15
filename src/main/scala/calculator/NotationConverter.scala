package main.scala.calculator

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

object NotationConverter {
  // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  val Precedence = Map(
    '(' -> 1, ')' -> 1,
    '*' -> 3, '/' -> 3,
    '+' -> 4, '-' -> 4
  )
  val Operators = Precedence.keys.toList

  def FromInfixToPostfix(operands: List[Char]): Either[List[Char], ParsingError] = {
    val result = ListBuffer[Char]()
    val operatorsStack = Stack[Char]()
    operands.foreach(token => {
      token match {
        case number if number isDigit => result += number
        case '(' => operatorsStack.push(token)
        case ')' => {
          while (!operatorsStack.isEmpty && operatorsStack.head != '(') {
            result += operatorsStack.pop()
          }
          if (operatorsStack.isEmpty) {
            return Right(new ParsingError(s"Parentheses are not balanced"))
          }
          operatorsStack.pop()
        }
        case operator if Operators contains operator => {
          if (!operatorsStack.isEmpty && operatorsStack.head != '(' && Precedence(operator) >= Precedence(operatorsStack.head)) {
            result += operatorsStack.pop()
          }
          operatorsStack.push(operator)
        }
        case _ => return Right(new ParsingError(s"Failed to parse '${token}' token"))
      }
    })
    result ++= operatorsStack.toList
    Left(result.toList)
  }
}
