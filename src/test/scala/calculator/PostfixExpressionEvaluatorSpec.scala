package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class PostfixExpressionEvaluatorSpec extends FlatSpec {
  behavior of "PostfixExpressionEvaluator evaluate method"

  private def Tokenize(input: String): List[Token] = Tokenizer.tokenize(input).left.get

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
  it should "sum and subtract positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1 4 + 3 -")) == Left(2.0))
  }

  it should "multiply and divide positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("0.8 4 * 32 /")) == Left(0.1))
  }

  it should "sum and multiply positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1.25 3 11.25 * +")) == Left(35.0))
  }

  it should "divide and subtract positive numbers with positive result" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("60.003 3 / 1 -")) == Left(19.001))
  }

  //invalid expressions
  it should "return an error when can't find sufficient number of operator parameters" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("4+")) == Right(new ExpressionError("Insufficient number of operands for '+':1 operator")))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("34--")) == Right(new ExpressionError("Insufficient number of operands for '-':2 operator")))
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1 2 * /")) == Right(new ExpressionError("Insufficient number of operands for '/':6 operator")))
  }

  it should "return an error when operator is unknown" in {
    assert(PostfixExpressionEvaluator.evaluate(List[Token](
      new Token("1", TokenKind.Number, 0),
      new Token("2", TokenKind.Number, 1),
      new Token("~", TokenKind.Operator, 2)
    )) == Right(new ExpressionError("Failed to parse '~':2 token")))
  }

  it should "return an error when few operands are left after evaluation" in {
    assert(PostfixExpressionEvaluator.evaluate(Tokenize("1 2")) == Right(new ExpressionError("Invalid expression")))
  }

  // TODO check numbers parsing
}
