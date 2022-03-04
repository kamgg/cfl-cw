// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// class definitions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp

// question 3
case class RANGE(cs: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, m: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

// question 4
case class CFUN(f: Char => Boolean) extends Rexp

// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true

  // question 3
  case RANGE(_) => false
  case PLUS(r) => nullable(r)
  case OPTIONAL(_) => true
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
  case UPTO(_, _) => true
  case FROM(r, n) => if (n == 0) true else nullable(r)
  case BETWEEN(r, n, m) => if (n == 0) true else nullable(r)
  case NOT(r) => !nullable(r)

  // question 4
  case CFUN(f) => false
}

// the derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2)) else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))

  // question 3
  case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r) => der(c, r)
  case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
  case UPTO(r, m) => if (m == 0) ZERO else SEQ(der(c, r), UPTO(r, m - 1))
  case FROM(r, n) => if (n == 0) der(c, STAR(r)) else SEQ(der(c, r), FROM(r, n - 1))
  case BETWEEN(r, n, m) => if (n == 0) UPTO(r, m  - 1) else SEQ(der(c, r), BETWEEN(r, n - 1, m - 1))
  case NOT(r) => NOT(der(c, r))

  // question 4
  case CFUN(f) => if (f(c)) ONE else ZERO
}

// question 6
def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
} 

// the derivative w.r.t. a string (iterates der)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}


// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))

// question 4
def CFUN_CHAR(c: Char) : Rexp = CFUN((d: Char) => c == d)
def CFUN_RANGE(cs: Set[Char]) : Rexp = CFUN((d: Char) => cs.contains(d))
def ALL() : Rexp = CFUN((d: Char) => true)

val regexps = List(
  OPTIONAL(CFUN_CHAR('a')),
  NOT(CFUN_CHAR('a')),
  NTIMES(CFUN_CHAR('a'), 3),
  NTIMES(OPTIONAL(CFUN_CHAR('a')), 3),
  UPTO(CFUN_CHAR('a'), 3),
  UPTO(OPTIONAL(CFUN_CHAR('a')), 3),
  BETWEEN(CFUN_CHAR('a'), 3, 5),
  OPTIONAL(BETWEEN(CFUN_CHAR('a'), 3, 5)),
  PLUS(CFUN_CHAR('a')),
  NTIMES(CFUN_CHAR('a'), 0)
)

val strings = List(
  "",
  "a",
  "aa",
  "aaa",
  "aaaaa",
  "aaaaaa",
  "aaaaaaa"
)
@main
// question 3
def q3() = {
  for (i <- 0 to regexps.size - 1) {
    for (j <- 0 to strings.size - 1) {
      println(f"Testing ${regexps(i)} against ${strings(j)}")
      println(f"Result: ${matcher(regexps(i), strings(j))}")
    }
  }
}

@main
// question 5
def email(email: String) = {
  val first = Set(
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '_',
    '-',
    '.'
  )

  val second = Set(
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '-',
    '.'
  )

  val third = Set(
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '.'
  )

  val emailRexp = SEQ(PLUS(RANGE(first)), SEQ(CHAR('@'), SEQ(PLUS(RANGE(second)), SEQ(CHAR('.'),  BETWEEN(RANGE(third), 2, 6)))))
  println(ders(email.toList, emailRexp))
  println(matcher(emailRexp, email))
}

@main
// question 6
def q6() = {
  val rexp = SEQ(CHAR('/'), SEQ(CHAR('*'), SEQ(NOT(SEQ(STAR(ALL()), SEQ(CHAR('*'), SEQ(CHAR('/'), STAR(ALL()))))), SEQ(CHAR('*'), CHAR('/')))))

  val cases = List(
    "/**/",
    "/*foobar*/",
    "/*test*/test*/",
    "/*test/*test*/"
  )

  for (i <- 0 to cases.size - 1) {
    println(matcher(rexp, cases(i)))
  }
}


@main
// question 7
def q7() = {
  val r1 = SEQ(CHAR('a'), SEQ(CHAR('a'), CHAR('a')))

  val r2 = SEQ(BETWEEN(CHAR('a'), 19, 19), OPTIONAL(CHAR('a')))

  val cases = List(
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  )
    for (i <- 0 to cases.size - 1) {
    println("r1: " + matcher(PLUS(PLUS(r1)), cases(i)))
    println("r2: " + matcher(PLUS(PLUS(r2)), cases(i)))
  }
}