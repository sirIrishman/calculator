package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.NotationConverter

class NotationConverterSpec extends FlatSpec {
  behavior of "NotationConverter FromInfixToPostfix method"

  // operators
  it should "process operators with the same precedence: +, -" in {
    val infix = "1+2-3".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left("12+3-".toList))
  }

  it should "process operators with the same precedence: *, /" in {
    val infix = "1*4/2".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('1', '4', '*', '2', '/')))
  }

  it should "process operators with a different precedence: +, *" in {
    val infix = "1+3*4".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('1', '3', '4', '*', '+')))
  }

  it should "process operators with a different precedence: /, -" in {
    val infix = "8/2-2".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('8', '2', '/', '2', '-')))
  }

  // parentheses + operators
  it should "process parentheses and operators with the same precedence: +, -" in {
    val infix = "1+(4-3)".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('1', '4', '3', '-', '+')))
  }

  it should "process parentheses and operators with the same precedence: *, /" in {
    val infix = "1*(4/2)".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('1', '4', '2', '/', '*')))
  }

  it should "process parentheses and operators with a different precedence: +, *" in {
    val infix = "(1+3)*4".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('1', '3', '+', '4', '*')))
  }

  it should "process parentheses and operators with a different precedence : /, -" in {
    val infix = "8/(4-2)".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(List[Char]('8', '4', '2', '-', '/')))
  }
}
