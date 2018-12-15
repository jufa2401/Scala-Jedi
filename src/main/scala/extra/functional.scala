package jedi.util

object functional {
  def tap[T](effect: T => Unit)(x: T) = {
    effect(x)
    x
  }
}
