package com.noisycode

trait Comparisons {
  this: Evaluator =>

  val gt: pf =
    { case List(Id(">"), a, b) => doCompare(List(a, b), ((x, y) => Bool(x.v > y.v))) }
  val lt: pf =
    { case List(Id("<"), a, b) => doCompare(List(a, b), ((x, y) => Bool(x.v < y.v))) }
  val eq: pf =
    { case List(Id("="), a, b) => doCompare(List(a, b), ((x, y) => Bool(x.v == y.v))) }

  def doCompare(t: List[Term], m: (Number, Number) => Bool): Term = {
    t match {
      case List(a, b) => (resolveTerm(a), resolveTerm(b)) match {
	case (Number(x), Number(y)) => m(Number(x), Number(y))
	case _ => Error("Could not resolve on or both to numbers:  " + t.toString)
      }
    }
  }
}
