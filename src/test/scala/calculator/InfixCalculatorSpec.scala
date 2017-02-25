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
    assert(new InfixCalculator().calculate("-86") == Left("-86.0"))
    assert(new InfixCalculator().calculate("-590.001") == Left("-590.001"))
  }

  it should "return number value of single input constant" in {
    assert(new InfixCalculator().calculate("pi") == Left("3.141592653589793"))
    assert(new InfixCalculator().calculate("Pi") == Left("3.141592653589793"))
    assert(new InfixCalculator().calculate("PI") == Left("3.141592653589793"))
    assert(new InfixCalculator().calculate("-pi") == Left("-3.141592653589793"))

    assert(new InfixCalculator().calculate("e") == Left("2.718281828459045"))
    assert(new InfixCalculator().calculate("E") == Left("2.718281828459045"))
    assert(new InfixCalculator().calculate("-e") == Left("-2.718281828459045"))
  }

  it should "apply all supported mathematical operations without grouping" in {
    assert(new InfixCalculator().calculate("1^100/2+-pi-4*-1-e+11%4*-0.5") == Left("-2.859874482048838"))
    assert(new InfixCalculator().calculate("1 ^ 100 / 2 + -pi - 4 * -1 - e + 11 % 4 * -0.5") == Left("-2.859874482048838"))
  }

  it should "apply all supported mathematical operations with grouping" in {
    assert(new InfixCalculator().calculate("((2^3-14)*(Pi+0.48+0.02-Pi))/(504%(e+99+-e+2))") == Left("-0.03"))
    assert(new InfixCalculator().calculate("((2 ^ 3 - 14) * (Pi + 0.48 + 0.02 - Pi)) / (504 % (e + 99 + -e + 2))") == Left("-0.03"))
  }

  // TODO test error cases
}
