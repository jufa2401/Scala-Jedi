package context

import expression._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Expression] = "while" ~ "(" ~> expression ~ ")" ~ expression ^^ {
    case condition ~ ")" ~ body => Iteration(condition, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case exp => FunCall(Identifier("dereference"), List(exp))
    case _ => throw new JediException("Error dereferencing")
  }

  override def term: Parser[Expression]  = thunk | lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")

}