package com.noisycode

import scala.util.parsing.combinator._


object SExpParser extends RegexParsers {
  def number: Parser[Number] = """-?\d+(\.\d+)?""".r ^^ { d => Number(d.toDouble) }
  def chr: Parser[BadChar] =  "'" ~> """[^']""".r <~ "'" ^^ { c => BadChar(c.head) }
  def str: Parser[BadString] = ("\"" ~> """[^\"]+""".r <~ "\"") ^^ { s => BadString(s) }
  def boolean: Parser[Bool] = ("true" | "false") ^^ { b => Bool(b.toBoolean) }

  def id: Parser[Id] = """[a-zA-Z\*\/\+\-<>=?!]+[a-zA-Z\*\/\+\-<>=0-9?!]*""".r ^^ { i => Id(i) }

  def value: Parser[Value] = number | chr | str

  def data: Parser[Data] = "'(" ~> rep(value | id | sexp) <~ ")" ^^ { l => Data(l) }
  def sexp: Parser[SExp] = "(" ~> rep(value | id | sexp | data) <~ ")" ^^ { l => SExp(l) }

  def badList: Parser[ListTerm] = data | sexp
  def program: Parser[List[SExp]] = rep(sexp)

  def parseLine(expression: String) = parseAll(badList, expression)
  def parseProgram(source: String) = parseAll(program, source)
}
