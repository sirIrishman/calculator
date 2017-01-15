package main.scala.calculator

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

object NotationConverter {
  // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
  val Precedence = Map(
    '*' -> 3, '/' -> 3,
    '+' -> 4, '-' -> 4
  )
  val Operators = Precedence.keys.toList

  def FromInfixToPostfix(operands: List[Char]): List[Char] = {
    val result = ListBuffer[Char]()
    val operatorsStack = Stack[Char]()
    operands.foreach(x => {
      x match {
        case number if number isDigit => result += number
        case operator if Operators contains operator => {
          if (!operatorsStack.isEmpty && Precedence(operator) >= Precedence(operatorsStack.head)) {
            result += operatorsStack.pop()
          }
          operatorsStack.push(operator)
        }
        //case _ => throw
      }
    })
    result ++= operatorsStack.toList
    result.toList
  }
}
