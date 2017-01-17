package main.scala.calculator

import scala.collection.mutable.ListBuffer

object Tokenizer {

  object CharType extends Enumeration {
    type CharType = Value
    val Space, Digit, DecimalMark, Parenthesis = Value
  }

  import CharType._

  def Tokenize(input: String): Either[List[Token], ExpressionError] = {
    val result = ListBuffer[Token]()
    val accumulator = ListBuffer[(Char, CharType, Int)]()
    for ((currentChar: Char, index: Int) <- input.view.zipWithIndex) {
      val current = (currentChar, GetCharType(currentChar), index)
      val nextCharType = if (index + 1 == input.length) None else Some(GetCharType(input(index + 1)))
      (current._2, nextCharType) match {
        case (Space, _) => {
          if (!accumulator.isEmpty) {
            result += new Token(accumulator.map(x => x._1) mkString, accumulator.head._3)
            accumulator.clear()
          }
        }
        case (Digit, Some(Digit)) | (Digit, Some(DecimalMark)) | (DecimalMark, Some(Digit)) =>
          accumulator += current
        case (_, Some(Space)) | (_, None) | (_, Some(Parenthesis)) | (Parenthesis, _) => {
          accumulator += current
          result += new Token(accumulator.map(x => x._1) mkString, accumulator.head._3)
          accumulator.clear()
        }
        case _ =>
          return Right(new ExpressionError(s"Failed to parse '${(accumulator += current) mkString}' token"))
      }
    }
    Left(result.toList)
  }

  private def GetCharType(char: Char): CharType = {
    return char match {
      case _ if char isDigit => Digit
      case _ if char isSpaceChar => Space
      case '.' => DecimalMark
      case '(' | ')' => Parenthesis
    }
  }
}
