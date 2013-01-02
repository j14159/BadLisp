package com.noisycode

object Repl {
  def main(args: Array[String]) {
    new Repl().run()
  }
}

class Repl {
  jline.Terminal.setupTerminal().disableEcho()

  val reader = new jline.ConsoleReader()

  val eval = new BadLispEval()
  
  def run() {
    var line = reader.readLine("BadLisp:  ")
    while((line != "quit") && (line != "exit")) {
      SExpParser.parseSource(line) match {
	case SExpParser.Success(result, _) => println(eval.eval(result))
	case somethingElse => println("Parsing failed:  " + somethingElse.toString)
      }

      line = reader.readLine()
    }
  }
}
