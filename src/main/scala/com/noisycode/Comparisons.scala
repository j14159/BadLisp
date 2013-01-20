package com.noisycode

trait Comparisons {
  this: Evaluator =>

  val gt: pf =
    { case List(Id(">"), a: Term, b: Term) => resolveTerm(a) > resolveTerm(b) }
  val lt: pf =
    { case List(Id("<"), a: Value, b: Value) => resolveTerm(a) < resolveTerm(b) }
  val eq: pf =
    { case List(Id("="), a: Term, b: Term) => resolveTerm(a) badEq resolveTerm(b) }

  implicit class ComparableValue(v: Value) {
    def >(v2: Value): Value = {
      (v, v2) match {
	case (Number(x), Number(y)) => Bool(x > y)
	case (BadChar(x), BadChar(y)) => Bool(x > y)
	case (BadString(x), BadString(y)) => Bool(x > y)
	case (x, y) => Error(s"Can't compare $x and $y with >")
      }
    }

    def <(v2: Value): Value = {
      (v, v2) match {
	case (Number(x), Number(y)) => Bool(x < y)
	case (BadChar(x), BadChar(y)) => Bool(x < y)
	case (BadString(x), BadString(y)) => Bool(x < y)
	case (x, y) => Error(s"Can't compare $x and $y with <")
      }
    }

    def badEq(v2: Value): Value = Bool(v == v2)
  }
}
