package com.noisycode

/**
 * Provides support for using "define" to create global values and functions.
 */
trait Definitions {
  this: Evaluator =>

  val constant: pf =
    {
      case List(Id("define"), Id(id), other) => {
        resolveTerm(other) match {
          case v: Value => {
            val resolverFunc: pf = {
              case List(Id(id)) => v
            }

            symbolTable = resolverFunc :: symbolTable
            Sym(id, v)
          }
          case _ => Error("SExp in define of non-function must result in a concrete value:  " + other.toString)
        }
      }
    }
  val function: pf =
    {
      case List(Id("define"), SExp(params: List[Id]), SExp(body)) => {
        val funcName = params.head
        val parameters = params.tail.map(_.id)

        val thisFunc: pf = {
          case (Id(fn) :: rest) if rest.length == parameters.length && fn == funcName.id => {
            val resolvedArgs = rest.map {
              p => resolveTerm(p)
            }

            val localBindings: List[pf] = parameters.zip(resolvedArgs).map {
              tup =>
                {
                  val resolverFunc: pf = {
                    case List(Id(tup._1)) => tup._2
                  }
                  resolverFunc
                }
            }

            /*
	     * This is kludgy but works, doesn't get stack overflows
	     * on basic recursive functions like the old "new evaluator"
	     * method did.  Probably better to have a definition of eval()
	     * that takes symbol table arguments or something like that.
	     */
            val old = symbolTable
            symbolTable = localBindings ::: symbolTable
            val result = eval(body)
            symbolTable = old
            result
          }
        }
        symbolTable = thisFunc :: symbolTable

        Func(params.head.id, params.tail, SExp(body))
      }
    }
}
