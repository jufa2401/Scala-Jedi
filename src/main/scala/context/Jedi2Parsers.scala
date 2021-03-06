package context
import expression._

class Jedi2Parsers extends Jedi1Parsers {


  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(con ~ Nil) => List(con)
    case Some(con ~ more) => con::more
    case _ => Nil
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ pars ~ exps => Lambda(pars, exps)
  }

  // block parser
  // a block is one or more semi-co lon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ e ~ Nil ~ "}" => Block(List(e))
    case "{" ~ e ~ expressions ~ "}" => Block(e::expressions)
  }

  def thunk: Parser[MakeThunk] = "freeze(" ~ expression ~ ")" ^^ {
    case "freeze(" ~ expression ~ ")" => MakeThunk(expression)
  }

  // override of term parser
  override def term: Parser[Expression]  = lambda | thunk | funCall | block | literal | "("~>expression<~")"

}