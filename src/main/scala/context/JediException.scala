package context

import expression.Identifier

import scala.util.parsing.combinator._

class JediException(val gripe: String = "Jedi error ") extends Exception(gripe)
class BreakException(gripe: String = "DONE") extends JediException(gripe)
class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name)
class TypeException(gripe: String = "Type Error") extends JediException(gripe)
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")