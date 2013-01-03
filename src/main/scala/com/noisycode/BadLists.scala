package com.noisycode

trait BadLists {
  this: Evaluator =>

  val cons: pf =
    {
      case List(Id("cons"), t: Term, backend) => resolveTerm(backend) match {
        case n: Number => Data(List(t, n))
        case Data(d) => Data(t :: d)
        case other => Error("Not something you can cons onto:  " + other.toString)
      }
    }
  val car: pf =
    {
      case List(Id("car"), check) => resolveTerm(check) match {
        case Data(d) => resolveTerm(d.head)
        case other => Error("car needs a list, not " + other.toString)
      }
    }
  val cdr: pf =
    {
      case List(Id("cdr"), check) => resolveTerm(check) match {
        case Data(d) => Data(d.tail)
        case other => Error("cdr needs a list, not " + other.toString)
      }
    }
}
