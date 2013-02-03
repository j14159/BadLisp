BadLisp
=======

Simple/incomplete/bad LISP experiment in Scala.  This represents roughly 20 hours of work over a month (much of it back tracking) so you will probably find lots of weird errors in the dusty corners.

This is essentially a toy/learning experiment so please do not expect it to behave even remotely like production/robust software.

#Usage
Clone the repo and do `sbt run`.  BadLisp uses Scala 2.10.0 and SBT 0.12.1 but I think anything 0.11.x or 0.10.x and up should work.  Some basic help is printed including how to load external files.

#Why
See the [blog post](http://noisycode.com/blog/2013/02/02/parser-n00b/).  TLDR:  I got carried away.

#The Basics
Syntax is roughly modeled on Scheme but this isn't even remotely close to a full implemention.  

##Data Types
The following data types exist and can be used directly:

* Number (backed by doubles)
* Boolean
* String ("-enclosed)
* Character('-enclosed)
* Data (a '-prefixed S-expression)

There are a few others that can't really be manipulated directly:

* Function
* Error
* SExp (S-expression)

A couple of examples:

	BadLisp:  (car '(1 2 3))
	BadLisp:  (define x '(1 2 3))
	BadLisp:  (define (add x y) (+ x y))
	BadLisp:  (add 1 4)

##Special Forms

Pretty much just `define` and `if`:

	BadLisp:  (define (>2? x) (> x 2))
	BadLisp:  (define x 5)
	BadLisp:  (define (if>2 x) (if (>2? x) "yes" "no"))

##Built-In Functions
A list of functions that are built into the interpreter:

* +, -, /, *
* str, str-length
* cons, car, cdr
* >, <, =

#What You Won't Find
A non-exhaustive list:

* cond
* continuations
* any sort of IO
* any sort of concurrency at all

And probably many other things I haven't thought of.

#Etc.
See `example.badlisp` for implementations of length, map and reduce.

In all likelihood I won't be maintaining or fixing anything further.  The project has served its purpose (learning/experimentation) and when I want something more robust/complete, I'll probably just start from scratch.  Having said all that, feel free to ping me on [Twitter](http://twitter.com/j14159) if you have questions/comments about the code.