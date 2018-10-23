package value
import expression._

case class Chars(val value: String) extends Literal {
  override def toString: String = value

  def <(other: Chars): Boolean = value < other.value
  def ==(other: Chars): Boolean = value.equals(other.value)
  def +(other: Chars) = Chars(value + other.value)
  def substring(x: Integer, y: Integer = Integer(value.length())) = Chars(value.substring(x.value,y.value))
}