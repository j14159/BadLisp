package com.noisycode

/**
 * Provides support for using "define" to create global values and functions.
 */
trait Definitions {
  this: TypeHelper with Bindings =>

  //need eval for the global value stuff:
  def eval(s: SExp): Term
  def eval(t: List[Term]): Term

  //need the symbol table to be able to register partial functions for function execution:
  var symbolTable: List[pf]

  val constant: pf =
    {
      case List(Id("define"), Id(id), Number(value)) => {
        bindings = bindings + (id -> Number(value))
        Sym(id, Number(value))
      }
      case List(Id("define"), Id(id), SExp(generator)) => {
        eval(generator) match {
          case Number(n) => {
            bindings = bindings + (id -> Number(n))
            Sym(id, Number(n))
          }
          case _ => Error("SExp in define of non-function must result in a concrete value:  " + generator.toString)
        }
      }
    }
  val function: pf =
    {
      case List(Id("define"), SExp(params: List[Id]), SExp(body)) => {
        val funcName = params.head
        val parameters = params.tail.map(_.v)

        val thisFunc: pf = {
          case (Id(fn) :: rest) if rest.length == parameters.length && fn == funcName.v => {
	    val resolvedArgs = rest.map {
	      p => p match {
		case Id(id) => bindings(id)
		case other => other
	      }
	    }

	    val totalBindings = parameters.zip(resolvedArgs).toMap ++ bindings
            new BadLispEval(totalBindings, symbolTable).eval(body)
          }
        }
        symbolTable = thisFunc :: symbolTable

        Func(params.head.v, params.tail, SExp(body))
      }
    }
}
