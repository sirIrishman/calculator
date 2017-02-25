package main.scala.calculator

import scala.collection.mutable.ListBuffer

object InfixTokenizer {

  private object CharType extends Enumeration {
    type CharType = Value
    val Space, Digit, DecimalMark, Operator, Letter = Value
  }

  import CharType._

  def tokenize(expression: String): Either[List[Token], ExpressionError] = {
    val accumulator = ListBuffer[(Char, CharType, Int)]()

    def createTokenFromAccumulator(): Either[Token, ExpressionError] = {
      val text: String = accumulator.map(x => x._1) mkString
      val position: Int = accumulator.head._3
      val token = createToken(accumulator.map(x => x._2).toList, text, position)
        .getOrElse(return Right(new ExpressionError(s"Failed to parse '${text}':${position} token")))
      Left(token)
    }

    val tokens = ListBuffer[Token]()
    for ((currentChar: Char, index: Int) <- expression.view.zipWithIndex) {
      val current = (currentChar, getCharType(currentChar), index)
      val nextCharType = if (index + 1 == expression.length) None else Some(getCharType(expression(index + 1)))
      (current._2, nextCharType) match {
        case (Space, _) => {
          //do nothing
        }
        case (Operator, Some(Digit)) |
             (Operator, Some(Letter))
          if currentChar == '-' && (tokens.isEmpty || tokens.last.isInstanceOf[OperatorToken] && tokens.last.text != ")") =>
          accumulator += current
        case (Digit, Some(Digit)) |
             (Digit, Some(DecimalMark)) |
             (DecimalMark, Some(Digit)) |
             (Letter, Some(Letter)) =>
          accumulator += current
        case (_, Some(Space)) |
             (_, Some(Operator)) |
             (_, Some(Letter)) |
             (_, None) |
             (Operator, _) |
             (Letter, Some(Digit)) => {
          accumulator += current
          createTokenFromAccumulator() match {
            case Left(newToken) => tokens += newToken
            case Right(error) => return Right(error)
          }
          accumulator.clear()
        }
        case _ =>
          accumulator += current
          return Right(new ExpressionError(s"Failed to parse '${accumulator.map(_._1) mkString}':${accumulator.head._3} token"))
      }
    }
    Left(tokens.toList)
  }

  private def getCharType(char: Char): CharType = {
    return char match {
      case _ if char isDigit => Digit
      case _ if char isSpaceChar => Space
      case _ if ('a' to 'z' contains char) || ('A' to 'Z' contains char) => Letter
      case '.' => DecimalMark
      case '(' | ')' | '+' | '-' | '*' | '/' | '^' | '%' => Operator
    }
  }

  private def createToken(charTypes: List[CharType], text: String, position: Int): Option[Token] = {
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
        Some(NumberToken(text, position))
      case Operator :: Nil =>
        Some(OperatorToken(text, position))
      case Letter :: Nil |
           Operator :: Letter :: Nil =>
        Some(VariableToken(text, position))
      case _ => None
    }
  }
}
