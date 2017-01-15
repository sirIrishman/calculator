package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.Calculator

class CalculatorSpec extends FlatSpec {
  behavior of "Calculator Calc method"

  // operators
  it should "sum and subtract positive numbers with positive result" in {
    val input = "1+4-3"
    assert(new Calculator().CalcInfix(input) == Left("2"))
  }

  it should "multiply and divide positive numbers with positive result" in {
    val input = "2*3/6"
    assert(new Calculator().CalcInfix(input) == Left("1"))
  }

  it should "sum and multiply positive numbers with positive result" in {
    val input = "2+3*2"
    assert(new Calculator().CalcInfix(input) == Left("8"))
  }

  it should "divide and divide positive numbers with positive result" in {
    val input = "6/3-1"
    assert(new Calculator().CalcInfix(input) == Left("1"))
  }

  // parentheses + operators
  it should "sum and subtract grouped positive numbers with positive result" in {
    val input = "(1+4)-3"
    assert(new Calculator().CalcInfix(input) == Left("2"))
  }

  it should "multiply and divide grouped positive numbers with positive result" in {
    val input = "2*(3/3)"
    assert(new Calculator().CalcInfix(input) == Left("2"))
  }

  it should "sum and multiply grouped positive numbers with positive result" in {
    val input = "(1+3)*2"
    assert(new Calculator().CalcInfix(input) == Left("8"))
  }

  it should "divide and divide grouped positive numbers with positive result" in {
    val input = "6/(3-1)"
    assert(new Calculator().CalcInfix(input) == Left("3"))
  }
}
