package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.NotationConverter

class NotationConverterSpec extends FlatSpec {
  behavior of "NotationConverter FromInfixToPostfix method"

  it should "process operators with the same precedence: +, +" in {
    val infix = "1+2+3".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('1', '2', '+', '3', '+'))
  }

  it should "process operators with the same precedence: -, -" in {
    val infix = "5-1-4".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('5', '1', '-', '4', '-'))
  }

  it should "process operators with the same precedence: +, -" in {
    val infix = "1+2-3".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('1', '2', '+', '3', '-'))
  }

  it should "process operators with the same precedence: *, *" in {
    val infix = "2*3*5".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('2', '3', '*', '5', '*'))
  }

  it should "process operators with the same precedence: /, /" in {
    val infix = "6/3/2".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('6', '3', '/', '2', '/'))
  }

  it should "process operators with the same precedence: *, /" in {
    val infix = "1*4/2".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('1', '4', '*', '2', '/'))
  }

  it should "process operators with a different precedence: +, *" in {
    val infix = "1+3*4".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('1', '3', '4', '*', '+'))
  }

  it should "process operators with a different precedence: /, -" in {
    val infix = "8/2-2".toList
    assert(NotationConverter.FromInfixToPostfix(infix) == List[Char]('8', '2', '/', '2', '-'))
  }
}
