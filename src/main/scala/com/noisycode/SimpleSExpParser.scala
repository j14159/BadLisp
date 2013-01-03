package com.noisycode

import scala.util.parsing.combinator._

sealed trait Term

sealed trait Value extends Term
sealed trait ListTerm

case class Number(v: Double) extends Value
case class Id(v: String) extends Term

case class Data(terms: List[Term]) extends Term with ListTerm with Value
case class SExp(terms: List[Term]) extends Term with ListTerm

case class Bool(b: Boolean) extends Term

case class Sym(id: String, value: Value) extends Term
case class Func(id: String, params: List[Id], body: SExp) extends Term

case class Error(msg: String) extends Term with Value

object SExpParser extends RegexParsers {
  def number: Parser[Number] = """-?\d+(\.\d+)?""".r ^^ { d => Number(d.toDouble) }
  def boolean: Parser[Bool] = ("true" | "false") ^^ { b => Bool(b.toBoolean) }
  def id: Parser[Id] = """[a-zA-Z\*\/\+\-<>=?!]+[a-zA-Z\*\/\+\-<>=0-9?!]*""".r ^^ { i => Id(i) }

  def data: Parser[Data] = "'(" ~> rep(number | id | sexp) <~ ")" ^^ { l => Data(l) }
  def sexp: Parser[SExp] = "(" ~> rep(number | id | sexp | data) <~ ")" ^^ { l => SExp(l) }

  def badList: Parser[ListTerm] = data | sexp

  def parseSource(expression: String) = parseAll(badList, expression)
}

object Example {
  def succeeds() = {
    val examples = List(
      "(> 0 9)",
      "(define (aFunction x y) (+ x y))",
      "(?test a 2)",
      "(some! (slightly? 0 (+ nested (stuff (* to (test 0))))))")

    examples map SExpParser.parseSource
  }

  def fails() = {
    SExpParser.parseSource("(unmatched (parens)")
  }
}
