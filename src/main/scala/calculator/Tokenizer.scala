package main.scala.calculator

import scala.collection.mutable.ListBuffer

object Tokenizer {

  private object CharType extends Enumeration {
    type CharType = Value
    val Space, Digit, DecimalMark, Operator = Value
  }

  import CharType._

  def tokenize(expression: String): Either[List[Token], ExpressionError] = {
    val accumulator = ListBuffer[(Char, CharType, Int)]()

    def createTokenFromAccumulator(): Either[Token, ExpressionError] = {
      val text: String = accumulator.map(x => x._1) mkString
      val position: Int = accumulator.head._3
      val tokenType = determineTokenType(accumulator.map(x => x._2).toList)
        .getOrElse(return Right(new ExpressionError(s"Failed to parse '${text}':${position} token")))
      Left(new Token(text, tokenType, position))
    }

    val tokens = ListBuffer[Token]()
    for ((currentChar: Char, index: Int) <- expression.view.zipWithIndex) {
      val current = (currentChar, getCharType(currentChar), index)
      val nextCharType = if (index + 1 == expression.length) None else Some(getCharType(expression(index + 1)))
      (current._2, nextCharType) match {
        case (Space, _) => {
          if (!accumulator.isEmpty) {
            createTokenFromAccumulator() match {
              case Left(newToken) => tokens += newToken
              case Right(error) => return Right(error)
            }
            accumulator.clear()
          }
        }
        case (Operator, Some(Digit)) if currentChar == '-' && (tokens.isEmpty || tokens.last.kind == TokenKind.Operator && tokens.last.text != ")") =>
          accumulator += current
        case (Digit, Some(Digit)) |
             (Digit, Some(DecimalMark)) |
             (DecimalMark, Some(Digit)) =>
          accumulator += current
        case (_, Some(Space)) |
             (_, Some(Operator)) |
             (_, None) |
             (Operator, _) => {
          accumulator += current
          createTokenFromAccumulator() match {
            case Left(newToken) => tokens += newToken
            case Right(error) => return Right(error)
          }
          accumulator.clear()
        }
        case _ =>
          return Right(new ExpressionError(s"Failed to parse '${(accumulator += current) mkString}':${accumulator.head._3} token"))
      }
    }
    Left(tokens.toList)
  }

  private def getCharType(char: Char): CharType = {
    return char match {
      case _ if char isDigit => Digit
      case _ if char isSpaceChar => Space
      case '.' => DecimalMark
      case '+' | '-' | '*' | '/' | '(' | ')' => Operator
    }
  }

  private def determineTokenType(charTypes: List[CharType]): Option[TokenKind.TokenKind] = {
    return charTypes.aggregate(List[CharType]())(
      (x, y) => {
        if ((x.isEmpty || x.last != y) && y != Nil) {
          x :+ y
        } else {
          x
        }
      },
      (x, y) => x ::: y
    ) match {
      case Digit :: Nil |
           Digit :: DecimalMark :: Digit :: Nil |
           Operator :: Digit :: Nil |
           Operator :: Digit :: DecimalMark :: Digit :: Nil =>
        Some(TokenKind.Number)
      case Operator :: Nil =>
        Some(TokenKind.Operator)
      case _ => None
    }
  }
}
