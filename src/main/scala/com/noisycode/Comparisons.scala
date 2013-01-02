package com.noisycode

trait Comparisons {
  this: TypeHelper with Bindings =>

  def eval(s: SExp): Term
  def eval(t: List[Term]): Term

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

  def resolveTerm(t: Term): Term = {
    t match {
      case Number(n) => Number(n)
      case Id(id) => bindings(id) match {
	case Number(n) => Number(n)
	case other => Error("Not a numerical binding:  " + other.toString)
      }
      case SExp(s) => eval(s) match {
	case Number(n) => Number(n)
	case other => Error("Could not resolve SExp to number:  " + s.toString)
      }
    }
  }
}
