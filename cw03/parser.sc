// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// Imports
import $file.lexer, lexer._
import ammonite.ops._
import scala.tools.nsc.interactive.Lexer.Token

// more convenience for the map parsers later on;
// it allows writing nested patterns as
// case x ~ y ~ z => ...

case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]

// Question 2
// Token type
type Tokens = Seq[(String, String)]

abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if tl.isEmpty) yield hd
}

// parser combinators

// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}

// atomic parser for (particular) strings/tokens
case class TokenParser(s: String) extends Parser[Tokens, String] {
  def parse(in: Tokens) = {
      if (!in.isEmpty && in.head._2 == s) Set((in.head._2, in.tail)) else Set()
  }
}
// Question 2
// atomic parser for identifiers (variable names)
case object IdentifierParser extends Parser[Tokens, String] {
  def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "Identifier") Set((in.head._2, in.tail)) else Set()
}

// atomic parser for numbers (transformed into ints)
case object NumberParser extends Parser[Tokens, Int] {
  def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "Number") Set((in.head._2.toInt, in.tail)) else Set()
}

case object StringParser extends Parser[Tokens, String] {
    def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "String") Set((in.head._2, in.tail)) else Set()
}

// the following string interpolation allows us to write 
// StrParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*) = TokenParser(sc.s(args:_*))
}    

// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

// Question 2
// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp

// Block
type Block = List[Stmt]

// Stmt
case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class WriteVar(s: String) extends Stmt
case class WriteStr(s: String) extends Stmt
case class Read(s: String) extends Stmt

// Arithmetic expressions
case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(a1: AExp, o: String,  a2: AExp) extends AExp // Arithmetic expressions (+, -, *, /, %)

// Booleannexpressions
case object True extends BExp
case object False extends BExp
case class Bop(a1: AExp, op: String, a2: AExp) extends BExp // boolean operations (>, <, ==, etc.)
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp

// Question 2
// arithmetic expressions inc. modulo
lazy val AExp: Parser[Tokens, AExp] = 
  (Te ~ p"+" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop(x, "+", z) } ||
  (Te ~ p"-" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop( x, "-", z) } || Te

lazy val Te: Parser[Tokens, AExp] = 
  (Fa ~ p"*" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop(x, "*", z) } || 
  (Fa ~ p"/" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop(x, "/", z) } || 
  (Fa ~ p"%" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop(x, "%", z) } || Fa

lazy val Fa: Parser[Tokens, AExp] = 
   (p"(" ~ AExp ~ p")").map{ case _ ~ y ~ _ => y } || 
   IdentifierParser.map(Var) || 
   NumberParser.map(Num)

// Question 2
// boolean expressions with complex nesting
lazy val BExp: Parser[Tokens, BExp] =
   (BoolTe ~ p"&&" ~ BExp).map[BExp]{ case x ~ _ ~ z => And(x, z) } ||
   (BoolTe ~ p"||" ~ BExp).map[BExp]{ case x ~ _ ~ z => Or(x, z) } || BoolTe

lazy val BoolTe: Parser[Tokens, BExp] =
   (AExp ~ p"==" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, "==", z) } || 
   (AExp ~ p"!=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, "!=", z) } || 
   (AExp ~ p"<" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, "<", z) } || 
   (AExp ~ p">" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, ">", z) } ||
   (AExp ~ p"<=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, "<=", z) } ||
   (AExp ~ p">=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(x, ">=", z) } || BoolFa

lazy val BoolFa: Parser[Tokens, BExp] =
   (p"true".map[BExp]{ _ => True }) || 
   (p"false".map[BExp]{ _ => False }) ||
   (p"(" ~ BExp ~ p")").map{ case _ ~ x ~ _ => x }

// lazy val BExp: Parser[Tokens, BExp] = 
//    (AExp ~ p"==" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } || 
//    (AExp ~ p"!=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } || 
//    (AExp ~ p"<" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } || 
//    (AExp ~ p">" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
//    (AExp ~ p"<=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
//    (AExp ~ p">=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">=", x, z) } ||
//    (p"(" ~ BExp ~ p")" ~ p"&&" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Lop("And", y, v) } ||
//    (p"(" ~ BExp ~ p")" ~ p"||" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Lop("Or", y, v) } ||
//    (p"true".map[BExp]{ _ => True }) || 
//    (p"false".map[BExp]{ _ => False }) ||
//    (p"(" ~ BExp ~ p")").map[BExp]{ case _ ~ x ~ _ => x }

// Question 2
// a single statement 
lazy val Stmt: Parser[Tokens, Stmt] =
  ((p"skip".map[Stmt]{_ => Skip }) ||
   (IdentifierParser ~ p":=" ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
   (IdentifierParser ~ p":=" ~ IdentifierParser).map[Stmt]{ case x ~ _ ~ z => Assign(x, Var(z)) } ||
   (p"write" ~ p"(" ~ IdentifierParser ~ p")").map[Stmt]{ case _ ~ _ ~ y ~ _ => WriteVar(y) } ||
   (p"write" ~ p"(" ~ StringParser ~ p")").map[Stmt]{ case _ ~ _ ~ y ~ _ => WriteStr(y) } ||

   (p"write" ~ IdentifierParser).map[Stmt]{ case _ ~ y => WriteVar(y) } ||
   (p"write" ~ StringParser).map[Stmt]{ case _ ~ y => WriteStr(y) } ||

   (p"read" ~ p"(" ~ IdentifierParser ~ p")").map[Stmt]{ case _ ~ _ ~ y => Read(y)} ||
   (p"read" ~ IdentifierParser).map[Stmt]{ case _ ~ y => Read(y)} ||

   (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
   (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) }) 
 
// statements
lazy val Stmts: Parser[Tokens, Block] =
  (Stmt ~ p";" ~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
  (Stmt.map[Block]{ s => List(s) })

// blocks (enclosed in curly braces)
lazy val Block: Parser[Tokens, Block] =
  ((p"{" ~ Stmts ~ p"}").map{ case _ ~ y ~ _ => y } || 
  (Stmt.map(s => List(s))))

// test boolean parser with complex nesting
// println(BExp.parse_all(lexing_simp(WHILE_REGEX, "true&&false")))
// println(BExp.parse_all(lexing_simp(WHILE_REGEX, "(true)&&false")))
// println(BExp.parse_all(lexing_simp(WHILE_REGEX, "(true)&&(false)")))
// println(BExp.parse_all(lexing_simp(WHILE_REGEX, "(1>2)&&true")))

// parse program, filtering whitespaces and comments
def parse(program : String) = {
  Stmts.parse_all(lexing_simp(WHILE_REGEX, program).filter {
    case (x, _) => x match {
      case "Whitespace" => false
      case "Comment" => false
      case _ => true
    }
  }).head
}

// Question 2 tests
@main
def q2test() = {
  val files = List("fib.while", "loops.while", "primes.while", "collatz.while")

  for (file <- files) {
    val test = read(pwd / file)
    println(s"==== '$file' =====")
    println(parse(test))
  }
}

@main
def q2estindividual(file : String) = {
  val test = read (pwd / file)
  println(s"==== '$file' =====")
  println(parse(test))
}

@main
def q2iftest() = {
    val test = "if (a<b) then skip else a :=a * b + 1"
    println(s"==== Question 2 if test =====")
  println(parse(test))
}

@main
def strtest(test : String) = {
  val parsetree = parse(test)

  parsetree(0) match {
    case WriteStr(x) => {
      print(StringContext.treatEscapes(x.substring(1, x.length - 1)))
    }
  }
}

@main
def booltest(bool : String) = {

  println(BExp.parse_all(lexing_simp(WHILE_REGEX, bool).filter {
    case (x, _) => x match {
      case "Whitespace" => false
      case "Comment" => false
      case _ => true
    }
  }).head)
}