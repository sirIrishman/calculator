package test

import org.scalatest.FlatSpec
import Calculator.Calculator

class CalculatorTests extends FlatSpec {
  behavior of "Calculator Calc method"

  it should "sum two positive integer numbers" in {
    val input = "1+2"
    assert(new Calculator().Calc(input) == 3.toString)
  }

  it should "subtract two positive integer numbers with positive result" in {
    val input = "5-1"
    assert(new Calculator().Calc(input) == 4.toString)
  }

  it should "multiply two positive integer numbers" in {
    val input = "2*3"
    assert(new Calculator().Calc(input) == 6.toString)
  }

  it should "divide two positive integer numbers with positive result" in {
    val input = "6/3"
    assert(new Calculator().Calc(input) == 2.toString)
  }
}
