package com.noisycode

import scala.annotation.tailrec

object Repl {
  def main(args: Array[String]) {
    new Repl().run("BadLisp:  ", "", 0)
  }
}

class Repl {
  jline.Terminal.setupTerminal().disableEcho()

  val reader = new jline.ConsoleReader()

  val eval = new BadLispEval()

  printHelp()

  @tailrec
  final def run(prompt: String, line: String, emptyLines: Int): Unit = {
    var inputLine = reader.readLine(prompt)
    val builder = new StringBuilder(line)
    builder append inputLine

    val (nextPrompt, nextEmptyLines) = {
      if((inputLine == null || inputLine.length == 0) && builder.length == 0) {
	("BadLisp:  ", 0)
      } else if(inputLine == null || inputLine.length == 0) {
	if(emptyLines == 0) {
	  println("Empty line, enter one more to discard previous input")
	  ("|\t\t", 1)
	} else {
	  println("Two empty lines entered, discarding previous input")
	  builder.delete(0, builder.length())
	  ("BadLisp:  ", 0)
	}
      } else if(inputLine.startsWith(":")) {
	commandProcessor(inputLine)
	builder.delete(0, builder.length())
	("BadLisp:  ", 0)
      } else if(balancedParens(builder.toString)) {
	SExpParser.parseLine(builder.toString) match {
	  case SExpParser.Success(result, _) => println(eval.eval(result))
	  case somethingElse => println("Parsing failed:  " + somethingElse.toString)
	}

	builder.delete(0, builder.length())
	("BadLisp:  ", 0)
      } else {
	("|\t\t", 0)
      }
    }

    run(nextPrompt, builder.toString, nextEmptyLines)
  }

  /**
   * Performs very minimal checking to ensure that the number of open and close parentheses match,
   * used to enable multi-line REPL input.
   * Does NOT perform any syntax checking.
   */
  def balancedParens(toCheck: String) = {
    val open = toCheck.foldLeft(0)((p, n) => if(n == '(') p + 1 else p)
    val closed = toCheck.foldLeft(0)((p, n) => if(n == ')') p + 1 else p)
    
    open == closed
  }

  def commandProcessor(command: String) {
    command.split(" ").toList match {
      case (":help" :: rest)  => printHelp()
      case List(":load", path: String) => {
	val inputFile = scala.io.Source.fromFile(path)
	val source = inputFile.mkString
	inputFile.close()
	 
	SExpParser.parseProgram(source) match {
	  case SExpParser.Success(result, _) => {
	    result map eval.eval
	    println("OK")
	  }
	  case other => println(s"Had trouble parsing source file:  $other")
	}
      }
      case (":quit" :: _) | (":exit" :: _) => System exit 0
      case _ => println("Unknown command.")
    }
  }
  
  def printHelp() {
    val help = """:help - print this help
:load <file> load and evaluate a file of S-Expressions
:quit exit REPL
:exit exit REPL
"""
    println(help)
  }
}
