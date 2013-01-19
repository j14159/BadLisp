package com.noisycode

trait Comparisons {
  this: Evaluator =>

  val gt: pf =
    { case List(Id(">"), a: Value, b: Value) => a > b }//doCompare(List(a, b), ((x, y) => Bool(x > y))) }
  val lt: pf =
    { case List(Id("<"), a: Value, b: Value) => a < b } //doCompare(List(a, b), ((x, y) => Bool(x.n < y.n))) }
  val eq: pf =
    { case List(Id("="), a: Value, b: Value) => a badEq b } //doCompare(List(a, b), ((x, y) => Bool(x.n == y.n))) }

  def doCompare(t: List[Term], m: (AnyVal, AnyVal) => Bool): Term = {
    t match {
      case List(a, b) => (resolveTerm(a), resolveTerm(b)) match {
	case (Number(x), Number(y)) => m(x, y)
	case (BadChar(x), BadChar(y)) => m(x, y)
	case _ => Error("Could not resolve on or both to numbers:  " + t.toString)
      }
    }
  }

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
	case (x, y) => Error(s"Can't compare $x and $y with >")
      }
    }

    def badEq(v2: Value): Value = {
      (v, v2) match {
	case (Number(x), Number(y)) => Bool(x == y)
	case (BadChar(x), BadChar(y)) => Bool(x == y)
	case (BadString(x), BadString(y)) => Bool(x == y)
	case (x, y) => Error(s"Can't compare $x and $y with >")
      }
    }

  }
}
