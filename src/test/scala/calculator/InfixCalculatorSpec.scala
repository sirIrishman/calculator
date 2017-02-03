package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.InfixCalculator

class InfixCalculatorSpec extends FlatSpec {
  behavior of "InfixCalculator calculate method"

  it should "return empty result for empty input" in {
    assert(new InfixCalculator().calculate(null) == Left("0.0"))
    assert(new InfixCalculator().calculate("") == Left("0.0"))
    assert(new InfixCalculator().calculate("   ") == Left("0.0"))
  }

  it should "return input single number without any change" in {
    assert(new InfixCalculator().calculate("0") == Left("0.0"))
    assert(new InfixCalculator().calculate("325") == Left("325.0"))
    assert(new InfixCalculator().calculate("13.45345") == Left("13.45345"))
  }

  it should "apply all supported mathematical operations without grouping" in {
    assert(new InfixCalculator().calculate("1^100/2-4*-1+11%4*-0.5") == Left("3.0"))
    assert(new InfixCalculator().calculate("1 ^ 100 / 2 - 4 * -1 + 11 % 4 * -0.5") == Left("3.0"))
  }

  it should "apply all supported mathematical operations with grouping" in {
    assert(new InfixCalculator().calculate("((2^3-14)*(0.48+0.02))/(504%(99+2))") == Left("-0.03"))
    assert(new InfixCalculator().calculate("((2 ^ 3 - 14) * (0.48 + 0.02)) / (504 % (99 + 2))") == Left("-0.03"))
  }
}
