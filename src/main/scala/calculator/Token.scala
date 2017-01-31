package main.scala.calculator

abstract class Token(val text: String, val position: Int) {
  override def toString(): String = s"'${text}':${position}"
}

case class NumberToken(override val text: String, override val position: Int) extends Token(text, position)

case class OperatorToken(override val text: String, override val position: Int) extends Token(text, position)
