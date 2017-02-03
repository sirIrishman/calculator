package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class PostfixExpressionEvaluatorSpec extends FlatSpec {
  behavior of "PostfixExpressionEvaluator evaluate method"

  private def Tokenize(postfixExpression: String): List[Token] =
    postfixExpression.split(" ").flatMap(element => InfixTokenizer.tokenize(element).left.get).toList

  // edge cases
  it should "return empty token list" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("")) == Left(0.0))
  }

  // single number
  it should "return input single number without any change" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("0")) == Left(0.0))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("325")) == Left(325.0))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("13.45345")) == Left(13.45345))
  }

  // operators
  it should "sum and subtract numbers" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1 4 + 3 -")) == Left(2.0))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("23.008 9.1897 - -44 +")) == Left(-30.1817))
  }

  it should "multiply and divide numbers" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("0.8 4 * 32 /")) == Left(0.1))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("32.02 -4.1 / 21 *")) == Left(-164.0048780487805))
  }

  it should "sum and multiply numbers" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1.25 3 11.25 * +")) == Left(35.0))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("-3.000 8 11.25 + *")) == Left(-57.75))
  }

  it should "divide and subtract numbers" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("60.003 3 / 1 -")) == Left(19.001))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("2.86 3.11 - 1 /")) == Left(-0.25))
  }

  it should "apply modulo and power" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("57 0.5 2 ^ %")) == Left(0.0))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("-0.5 14 11 % ^")) == Left(-0.125))
  }

  //invalid expressions
  it should "return an error when can't find sufficient number of operator parameters" in {
    assert(PostfixExpressionEvaluator.evaluate(InfixTokenizer.tokenize("4+").left.get) == Right(new ExpressionError("Insufficient number of operands for '+':1 operator")))
    assert(PostfixExpressionEvaluator.evaluate(InfixTokenizer.tokenize("34--").left.get) == Right(new ExpressionError("Insufficient number of operands for '-':2 operator")))
    assert(PostfixExpressionEvaluator.evaluate(InfixTokenizer.tokenize("1 2 * /").left.get) == Right(new ExpressionError("Insufficient number of operands for '/':6 operator")))
  }

  it should "return an error when operator is unknown" in {
    assert(
      PostfixExpressionEvaluator.evaluate(List[Token](NumberToken("1", 0), NumberToken("2", 1), OperatorToken("~", 2))) ==
        Right(new ExpressionError("Failed to parse '~':2 token"))
    )
  }

  it should "return an error when few operands are left after evaluation" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1 2")) == Right(new ExpressionError("Invalid expression")))
  }

  // TODO check numbers parsing
}
