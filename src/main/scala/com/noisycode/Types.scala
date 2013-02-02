package com.noisycode

sealed trait Term

sealed trait Value extends Term
sealed trait ListTerm

case class Number(n: Double) extends Value

case class BadChar(c: Char) extends Value
case class BadString(s: String) extends Value with ListTerm

case class Bool(b: Boolean) extends Value

case class Id(id: String) extends Term

case class Data(d: List[Term]) extends Term with ListTerm with Value
case class SExp(terms: List[Term]) extends Term with ListTerm

case class Sym(id: String, value: Value) extends Term
case class Func(id: String, params: List[Id], body: SExp) extends Value

case class Error(msg: String) extends Value

