// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// Imports
import $file.lexer, lexer._
import ammonite.ops._



// Token type
type Tokens = Seq[(String, String)]

abstract class Parser[I, T](implicit ev: I => Seq[_]) {
  def parse(ts: I): Set[(T, I)]

  def parse_single(ts: I) : T = 
    parse(ts).partition(_._2.isEmpty) match {
      case (good, _) if !good.isEmpty => good.head._1
      case (_, err) => { 
	println (s"Parse Error\n${err.minBy(_._2.length)}") ; sys.exit(-1) }
    }
}

// parser combinators
case class ~[+A, +B](x: A, y: B)

// sequence parser
class SeqParser[I, T, S](p: => Parser[I, T], 
                         q: => Parser[I, S])(implicit ev: I => Seq[_]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

// alternative parser
class AltParser[I, T](p: => Parser[I, T], 
                      q: => Parser[I, T])(implicit ev: I => Seq[_]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

// fun parser
class FunParser[I, T, S](p: => Parser[I, T], 
                         f: T => S)(implicit ev: I => Seq[_]) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

// more convenient syntax for parser combinators
implicit def ParserOps[I, T](p: Parser[I, T])(implicit ev: I => Seq[_]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
}

// list parser
def ListParser[I, T, S](p: => Parser[I, T], 
                        q: => Parser[I, S])(implicit ev: I => Seq[_]): Parser[I, List[T]] = {
  (p ~ q ~ ListParser(p, q)) ==> { case x ~ _ ~ z => x :: z : List[T] } ||
  (p ==> ((s) => List(s)))
}

// atomic parser for (particular) strings/tokens
case class TokenParser(s: String) extends Parser[Tokens, String] {
  def parse(in: Tokens) = {
      if (!in.isEmpty && in.head._2 == s) Set((in.head._2, in.tail)) else Set()
  }
}

// atomic parser for identifiers (variable names)
case object IdentifierParser extends Parser[Tokens, String] {
  def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "Identifier") Set((in.head._2, in.tail)) else Set()
}

// atomic parser for integers (transformed into ints)
case object IntParser extends Parser[Tokens, Int] {
  def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "Int") Set((in.head._2.toInt, in.tail)) else Set()
}

// atomic parser for doubles (transformed into ints)
case object DoubleParser extends Parser[Tokens, Float] {
  def parse(in: Tokens) = 
      if (!in.isEmpty && in.head._1 == "Double") Set((in.head._2.toFloat, in.tail)) else Set()
}

// atomic parser for non-void types
case object NVTypeParser extends Parser[Tokens, String] {
  def parse(in: Tokens) =
     if (!in.isEmpty && in.head._1 == "Type") Set((in.head._2, in.tail)) else Set()
}

// atomic parser for all types
case object TypeParser extends Parser[Tokens, String] {
  def parse(in: Tokens) =
     if (!in.isEmpty && (in.head._1 == "Void" || in.head._1 == "Type")) Set((in.head._2, in.tail)) else Set()
}

// the following string interpolation allows us to write 
// StrParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*) = TokenParser(sc.s(args:_*))
}

// the abstract syntax trees for the FUN language
abstract class Exp
abstract class BExp
abstract class Decl

// Decl
case class Def(name: String , args: List[(String , String)], ty: String , body: Exp) extends Decl
case class Main(e: Exp) extends Decl
case class Const(name: String , v: Int) extends Decl
case class FConst(name: String , x: Float) extends Decl

// Arithmetic and general expressions
case class Call(name: String, args : List[Exp]) extends Exp
case class Write(e: Exp) extends Exp
case class If(a: BExp, e1: Exp, e2: Exp) extends Exp

case class Var(s: String) extends Exp
case class Num(i: Int) extends Exp // integer numbers
case class FNum(i: Float) extends Exp // floating numbers
case class Aop(a1: Exp, o: String,  a2: Exp) extends Exp // Arithmetic expressions (+, -, *, /, %)

case class Sequence(e1: Exp, e2: Exp) extends Exp

// Boolean expressions
case class Bop(a1: Exp, op: String, a2: Exp) extends BExp // boolean operations (>, <, ==, etc.)


// Expressions
lazy val Exp: Parser[Tokens, Exp] = 
   (p"if" ~ BExp ~ p"then" ~ Exp ~ p"else" ~ Exp) ==> { case _ ~ x ~ _ ~ y ~ _ ~ z => If(x, y, z) : Exp } ||
   (M ~ p";" ~ Exp) ==> { case x ~ _ ~ y  => Sequence(x, y) : Exp } ||
   (p"{" ~ Exp ~ p"}") ==> { case _ ~ y ~ _ => y : Exp } ||
   M

lazy val M: Parser[Tokens, Exp] = 
   (p"write" ~ L) ==> { case _ ~ y => Write(y) : Exp } || L

lazy val L: Parser[Tokens, Exp] = 
   (Te ~ p"+" ~ Exp) ==> { case x ~ _ ~ z => Aop(x, "+", z) : Exp } ||
   (Te ~ p"-" ~ Exp) ==> { case x ~ _ ~ z => Aop( x, "-", z) : Exp } || Te

lazy val Te: Parser[Tokens, Exp] = 
   (Fa ~ p"*" ~ Te) ==> { case x ~ _ ~ z => Aop(x, "*", z) : Exp } || 
   (Fa ~ p"/" ~ Te) ==> { case x ~ _ ~ z => Aop(x, "/", z) : Exp } || 
   (Fa ~ p"%" ~ Te) ==> { case x ~ _ ~ z => Aop(x, "%", z) : Exp } || Fa

lazy val Fa: Parser[Tokens, Exp] = 
   (IdentifierParser ~ p"(" ~ ListParser(Exp, p",") ~ p")") ==> { case x ~ _ ~ z ~ _ => Call(x, z) : Exp } ||
   (IdentifierParser ~ p"(" ~ p")") ==> { case x ~ _ ~ _ => Call(x, List()) : Exp } || 
    (p"(" ~ Exp ~ p")") ==> { case _ ~ y ~ _ => y : Exp }||
   IdentifierParser ==> { case x => Var(x) : Exp } || 
   IntParser ==> { case x => Num(x) : Exp } || 
   DoubleParser ==> { case x => FNum(x) : Exp }

// Boolean expressions
lazy val BExp: Parser[Tokens, BExp] = 
   (Exp ~ p"==" ~ Exp) ==> { case x ~ _ ~ z => Bop(x, "==", z) : BExp } || 
   (Exp ~ p"!=" ~ Exp) ==> { case x ~ _ ~ z => Bop(x, "!=", z) : BExp } || 
   (Exp ~ p"<" ~ Exp) ==> { case x ~ _ ~ z => Bop(x, "<", z) : BExp } || 
   (Exp ~ p">" ~ Exp) ==> { case x ~ _ ~ z => Bop(z, "<", x) : BExp } ||
   (Exp ~ p"<=" ~ Exp) ==> { case x ~ _ ~ z => Bop(x, "<=", z) : BExp } ||
   (Exp ~ p">=" ~ Exp) ==> { case x ~ _ ~ z => Bop(z, "<=", x) : BExp }


lazy val Defn: Parser[Tokens, Decl] = 
   (p"def" ~ IdentifierParser ~ p"(" ~ ListParser(IdentifierParser ~ p":" ~ NVTypeParser, p",") ~ p")" ~ p":" ~ TypeParser ~ p"=" ~ Exp) ==> {
     case _ ~ x ~ _ ~ y ~ _ ~ _ ~ z ~ _ ~ w =>
      Def(x, y.map({case a ~ b ~ c => (a, c)}).toList, z, w) : Decl
   } ||
   (p"def" ~ IdentifierParser ~ p"(" ~ p")" ~ p":" ~ TypeParser ~ p"=" ~ Exp) ==> {
     case _ ~ x ~ _ ~ _ ~ _ ~ y ~ _ ~ z => Def(x, List(), y, z) : Decl
   } ||
   (p"val" ~ IdentifierParser ~ p":" ~ p"Double" ~ p"=" ~ DoubleParser) ==> {
     case _ ~ x ~ _ ~ _ ~ _ ~ y =>  FConst(x, y) : Decl
   } ||
   (p"val" ~ IdentifierParser ~ p":" ~ p"Int" ~ p"=" ~ IntParser) ==> {
     case _ ~ x ~ _ ~ _ ~ _ ~ y =>  Const(x, y) : Decl
   }


lazy val Prog: Parser[Tokens, List[Decl]] = 
    (Defn ~ p";" ~ Prog) ==> { case x ~ _ ~ z => x :: z : List[Decl] } ||
    (Exp ==> ((s) => List(Main(s)) : List[Decl]))


// parse program, filtering whitespaces and comments
def parse(program : String) = {
  Prog.parse_single(lexing_simp(FUN_REGEX, program).filter {
    case (x, _) => x match {
      case "Whitespace" => false
      case "Comment" => false
      case _ => true
    }
  })
}

@main
def deftest() = {
  val program = "val Ymin: Double = -1.3; def x_iter() : Void = {1 + 1; 1 + 1}; x_iter()" 
  println(parse(program))
}


@main
def parsefromfile(file : String) = {
  val test = read (pwd / file)
  
  println(s"==== '$file' ====")
  println(parse(test))
}