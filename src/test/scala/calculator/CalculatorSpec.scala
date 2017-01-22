package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.Calculator

class CalculatorSpec extends FlatSpec {
  behavior of "Calculator Calc method"

  // edge cases
  it should "return empty result for empty input" in {
    assert(new Calculator().calcInfix(null) == Left("0.0"))
    assert(new Calculator().calcInfix("") == Left("0.0"))
    assert(new Calculator().calcInfix("   ") == Left("0.0"))
  }

  // single number
  it should "return input single number without any change" in {
    assert(new Calculator().calcInfix("0") == Left("0.0"))
    assert(new Calculator().calcInfix("325") == Left("325.0"))
    assert(new Calculator().calcInfix("13.45345") == Left("13.45345"))
  }

  // operators
  it should "sum and subtract positive numbers with positive result" in {
    assert(new Calculator().calcInfix("1+4-3") == Left("2.0"))
  }

  it should "multiply and divide positive numbers with positive result" in {
    assert(new Calculator().calcInfix("2*3/6") == Left("1.0"))
  }

  it should "sum and multiply positive numbers with positive result" in {
    assert(new Calculator().calcInfix("2+3*2") == Left("8.0"))
  }

  it should "divide and divide positive numbers with positive result" in {
    assert(new Calculator().calcInfix("6/3-1") == Left("1.0"))
  }

  // parentheses + operators
  it should "sum and subtract grouped positive numbers with positive result" in {
    assert(new Calculator().calcInfix("(1+4)-3") == Left("2.0"))
  }

  it should "multiply and divide grouped positive numbers with positive result" in {
    assert(new Calculator().calcInfix("2*(3/3)") == Left("2.0"))
  }

  it should "sum and multiply grouped positive numbers with positive result" in {
    assert(new Calculator().calcInfix("(1+3)*2") == Left("8.0"))
  }

  it should "divide and divide grouped positive numbers with positive result" in {
    assert(new Calculator().calcInfix("6/(3-1)") == Left("3.0"))
  }
}
