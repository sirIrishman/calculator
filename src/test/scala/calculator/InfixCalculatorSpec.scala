package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.InfixCalculator

class InfixCalculatorSpec extends FlatSpec {
  behavior of "InfixCalculator calculate method"

  // edge cases
  it should "return empty result for empty input" in {
    assert(new InfixCalculator().calculate(null) == Left("0.0"))
    assert(new InfixCalculator().calculate("") == Left("0.0"))
    assert(new InfixCalculator().calculate("   ") == Left("0.0"))
  }

  // single number
  it should "return input single number without any change" in {
    assert(new InfixCalculator().calculate("0") == Left("0.0"))
    assert(new InfixCalculator().calculate("325") == Left("325.0"))
    assert(new InfixCalculator().calculate("13.45345") == Left("13.45345"))
  }

  // operators
  it should "sum, subtract, multiply and divide numbers without grouping" in {
    assert(new InfixCalculator().calculate("1/2-4*-1+3*-0.5") == Left("3.0"))
    assert(new InfixCalculator().calculate("1 / 2 - 4 * -1 + 3 * -0.5") == Left("3.0"))
  }

  // operators + grouping
  it should "sum, subtract, multiply and divide numbers with grouping" in {
    assert(new InfixCalculator().calculate("((8-14)*(0.48+0.02))/100") == Left("-0.03"))
    assert(new InfixCalculator().calculate("((8 - 14) * (0.48 + 0.02)) / 100") == Left("-0.03"))
  }
}
