package value

import expression.Literal

case class Chars(val value: String) extends Literal with Ordered[Chars] {
  override def toString: String = value

  override def <(other: Chars): Boolean = value < other.value
  def ==(other: Chars): Boolean = value.equals(other.value)
  def +(other: Chars) = Chars(value + other.value)
  def substring(x: Integer, y: Integer = Integer(value.length())) = Chars(value.substring(x.value,y.value))

  // TODO: Check if correct
  override def compare(other: Chars): Int = {
    if (this.value < other.value) -1
    else if (other.value < value) 1
    else 0
  }
}