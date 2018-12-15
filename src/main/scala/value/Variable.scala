package value
// is var since the content of value can change over time
class Variable(var content: Value) extends Value {
  override def toString = "[" + content.toString + "]"
}
// Companion
object Variable {
  def apply(content: Value): Variable = new Variable(content)
}