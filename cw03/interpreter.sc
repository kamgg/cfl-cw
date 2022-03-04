// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// Imports
import $file.lexer, lexer._
import $file.parser, parser._
import ammonite.ops._

// Question 3
// an interpreter for the WHILE language
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop(a1, "+", a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop(a1, "-", a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop(a1, "*", a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop(a1, "/", a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  case Aop(a1, "%", a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop(a1, "==", a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop(a1, "!=", a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(a1, ">", a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop(a1, "<", a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case Bop(a1, ">=", a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
  case Bop(a1, "<=", a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
  case While(b, bl) => if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env)) else env
  case WriteVar(x) => { println(env(x)) ; env } 
  case WriteStr(x) => {
    print(StringContext.treatEscapes(x.substring(1, x.length - 1)))
    env
  }
  case Read(x) => { env + (x -> Console.in.readLine().toInt) }
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())


// Question 3 tests
@main
def q3test() = {
  val files = List("fib.while", "loops.while", "primes.while", "collatz.while")

  for (file <- files) {
    val test = read(pwd / file)
    println(s"==== '$file' =====")
    println(eval(parse(test)))
  }
}

@main
def q3testindividual(file : String) = {
    val test = read(pwd / file)

    println(s"==== '$file' =====")
    println(eval(parse(test)))
}

// pulled from https://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * scala.math.pow(10, -9) + " s")
    result
}

@main
def q3testlooptime() = {
    val test = read(pwd / "loops.while")
    val result = time(eval(parse(test)))

    println(result)
}