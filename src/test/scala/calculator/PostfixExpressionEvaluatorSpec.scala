package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class PostfixExpressionEvaluatorSpec extends FlatSpec {
  behavior of "PostfixExpressionEvaluator Evaluate method"

  private def Tokenize(input: String): List[Token] = Tokenizer.Tokenize(input).left.get

  // edge cases
  it should "return empty token list" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("")) == Left(0.0))
  }

  // single number
  it should "return input single number without any change" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("0")) == Left(0.0))
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("325")) == Left(325.0))
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("13.45345")) == Left(13.45345))
  }

  // operators
  it should "sum and subtract positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("1 4 + 3 -")) == Left(2.0))
  }

  it should "multiply and divide positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("0.8 4 * 32 /")) == Left(0.1))
  }

  it should "sum and multiply positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("1.25 3 11.25 * +")) == Left(35.0))
  }

  it should "divide and divide positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.Evaluate(Tokenize("60.003 3 / 1 -")) == Left(19.001))
  }
}
