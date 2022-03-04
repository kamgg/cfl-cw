// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// Imports
import $file.lexer, lexer._
import $file.parser, parser._
import ammonite.ops._

// Generate a fresh label
var counter = -1
def Fresh(x: String) = {
    counter += 1
    x ++ "_" ++ counter.toString()
}

// Map of types to llvm types
val type_map = Map("Int" -> "i32", "Double" -> "double", "Void" -> "void")

// Internal CPS language for FUN
abstract class KExp
abstract class KVal

case class KVar(s: String, ty: String = "UNDEF") extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(i: Float) extends KVal
case class Kop(v1: KVal, o: String, v2: KVal) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal
case class KConst(s: String) extends KVal
case object KVoid extends KVal
case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}
case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KVoid(v: KVal, e: KExp) extends KExp
case class KReturn(v: KVal) extends KExp

// Map datatype to store type of constants and functions
type Types = Map[String, String]


// Typing function used to infer type of KVal
def typ_val(v: KVal, types: Types) : String = v match {
  case KVar(s, ty) => ty
  case KNum(i) => "Int"
  case KFNum(i) => "Double"
  case KCall(o, vrs) => types.getOrElse(o, "UNDEF")
  case KConst(s) => types.getOrElse(s"@$s", "UNDEF")
  case Kop(v1, o, v2) => {
    val ty1 = typ_val(v1, types)
    val ty2 = typ_val(v2, types)

    if (ty1 == "Double" || ty2 == "Double") {
      "Double"
    } else {
      "Int"
    }
  } 
}

// CPS translation from Exps to KExps using a continuation k.
def CPS(e: Exp, types: Types)(k: (KVal, Types) => (KExp, Types)) : (KExp, Types) = e match {
  case Var(s) => {
    val local_ty = types.getOrElse(s"%$s", "")
    val global_ty = types.getOrElse(s"@$s", "")

    if (local_ty != "") {
      k(KVar(s, local_ty), types)
    } else {
      if (global_ty != "") {
        val z = Fresh("tmp")
        val new_t = types + (s"%$z" -> global_ty)

        CPS(Var(z), new_t)((y1, t1) => {
          val result = k(KVar(z, global_ty), t1)

          (KLet(z, KConst(s), result._1), result._2)
        })
      } else {
        throw new Error(s"CPS Error: Variable '${s}' not defined.")
      }
    }
  }
  case Num(i) => k(KNum(i), types)
  case FNum(i) => k(KFNum(i), types)
  case Aop(e1, o, e2) => {
    val z = Fresh("tmp")

    CPS(e1, types)((y1, t1) => {
      CPS(e2, t1)((y2, t2) => {
        val ty = typ_val(Kop(y1, o, y2), t2)
        val new_t = t2 + (s"%$z" -> ty)

        val result = k(KVar(z, ty), new_t)

        (KLet(z, Kop(y1, o, y2), result._1), result._2)
      })
    })
  }
  case If(Bop(b1, o, b2), e1, e2) => {
    val z = Fresh("tmp")

    CPS(b1, types)((y1, t1) => {
      CPS(b2, t1)((y2, t2) => {
        val e1_result = CPS(e1, t2)(k)
        val e2_result = CPS(e2, e1_result._2)(k)

        (KLet(z, Kop(y1, o, y2), KIf(z, e1_result._1, e2_result._1)), t2)
      })
    })
  }
  case Call(name, args) => {
    def aux(args: List[Exp], types: Types, vrs: List[KVal]) : (KExp, Types) = args match {
      case Nil => {
        val ty = types.getOrElse(name, "")
        
        if (ty == "") {
          throw new Error(s"CPS Error: Function '${name}' not defined.")
        } else if(ty == "Void") {
          val result = k(KVoid, types)

          (KVoid(KCall(name, vrs), result._1), result._2)
        } else {
          val z = Fresh("tmp")
          val new_t = types + (s"%$z" -> ty)

          var result = k(KVar(z, ty), new_t)
          (KLet(z, KCall(name, vrs), result._1), result._2)
        }
      }
      case e::es => CPS(e, types)((y1, t1) => aux(es, t1, vrs ::: List(y1)))
    }
    aux(args, types, Nil)
  }
  case Sequence(e1, e2) => 
    CPS(e1, types)((y1, t1) => CPS(e2, t1)((y2, t2) => k(y2, t2)))
}

//initial continuation
def CPSi(e: Exp, types: Types) = CPS(e, types)((e1, t1) => (KReturn(e1), t1))

// convenient string interpolations 
// for instructions, labels and methods
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def string_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}

// mathematical and boolean operations
def compile_op(op: String) = op match {
  case "+" => "add i32 "
  case "*" => "mul i32 "
  case "-" => "sub i32 "
  case "/" => "sdiv i32 "
  case "%" => "srem i32 "
  case "==" => "icmp eq i32 "
  case "<=" => "icmp sle i32 "     // signed less or equal
  case "<"  => "icmp slt i32 "     // signed less than
}

def compile_dop(op: String) = op match {
  case "+" => "fadd double "
  case "*" => "fmul double "
  case "-" => "fsub double "
  case "/" => "fdiv double "
  case "%" => "frem double "
  case "==" => "fcmp oeq double "
  case "<=" => "fcmp ole double "
  case "<" => "fcmp olt double "
}


// compile K values
def compile_val(v: KVal, types: Types) : String = v match {
  case KNum(i) => s"$i"
  case KFNum(i) => s"$i"
  case KVar(s, ty) => s"%$s"
  case KConst(s) => s"load ${ type_map(typ_val(KConst(s), types)) }, ${ type_map(typ_val(KConst(s), types)) }* @$s"
  case Kop(x1, op, x2) => {
    val ty = typ_val(Kop(x1, op, x2), types)

    if (ty == "Double") {
      s"${compile_dop(op)} ${compile_val(x1, types)}, ${compile_val(x2, types)}" 
    } else {
      s"${compile_op(op)} ${compile_val(x1, types)}, ${compile_val(x2, types)}" 
    }
  }
  case KCall(x1, args) => {
    val ty = typ_val(KCall(x1, args), types)
    
    s"call ${type_map(ty)} @$x1(${ args.map({ case e1 => s"${ type_map(typ_val(e1, types)) } ${ compile_val(e1, types) }" }).mkString(", ") })"
  }
}

// compile K expressions
def compile_exp(a: KExp, types: Types) : String = a match {
  case KVoid(KCall(x1, args), e) => {
    i"call void @$x1(${ args.map({ case e1 => s"${ type_map(typ_val(e1, types)) } ${ compile_val(e1, types) }" }).mkString(", ") })" ++ compile_exp(e, types)
  }
  case KReturn(v) => {
    if (v == KVoid) {
      i"ret void"
    } else {
      val ty = typ_val(v, types)
      i"ret ${ type_map(ty) } ${ compile_val(v, types) }"
    }
  }
  case KLet(x, v, e) => 
    i"%$x = ${compile_val(v, types)}" ++ compile_exp(e, types)
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1, types) ++
    l"\n$else_br" ++ 
    compile_exp(e2, types)
  }
}

// compile function for declarations and main
def compile_decl(d: Decl, types: Types) : (String, Types) = d match {
  case Def(name, args, ty, body) => {
    val new_t = types + (name -> ty) ++ args.map(arg => (s"%${arg._1}" ->  arg._2))
    val cps = CPSi(body, new_t)
    
    (m"define ${ type_map(ty) } @$name (${ args.map({case (x, y) => s"${type_map(y)} %$x"}).mkString(", ") }) {" ++
    compile_exp(cps._1, cps._2) ++
    m"}\n", cps._2)
  }
  case Main(body) => {
    val cps = CPS(body, types)((e1, t1) => (KReturn(KNum(0)), t1))
    
    (m"define i32 @main() {" ++
    compile_exp(cps._1, cps._2) ++
    m"}\n", cps._2)
  }
  case Const(name, v) => {
    val new_t = types + (s"@$name" -> "Int")
    (m"@$name = global i32 $v", new_t)
  }
  case FConst(name, v) => {
    val new_t = types + (s"@$name" -> "Double")
    (m"@$name = global double $v", new_t)
  }
}

// Prelude
val header = """
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

@.str = private constant [4 x i8] c"%d\0A\00"

define void @print_int(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

; END OF BUILD-IN FUNCTIONS (header)
"""

// Included functions from header (prelude)
val include_types = Map(
  "printf" -> "Int",
  "new_line" -> "Void",
  "print_star" -> "Void",
  "print_space" -> "Void",
  "skip" -> "Void",
  "print_int" -> "Void"
)

// Compiler function which iterates through declarations and compiles each declaration, carrying type enviroment between iterations
def compile(prog: List[Decl], types: Types = include_types) : String = prog match {
  case head::tail => {
    val compiled = compile_decl(head, types)
    compiled._1 ++ compile(tail, compiled._2)
  }
  case Nil => ""
}

// Parse string then compile
def parsecompile(program: String) : String =
    header ++ compile(parse(program))


// val program = read(pwd / "mand.fun")

// println(parse(program))
// println(parsecompile(program))

@main
def compilefromfile(file : String) = {
    val program = read(pwd / file)
    val name = file.substring(0, file.indexOf("."))

    println(s"==== '$file' =====")
    println(parsecompile(program))
}

@main
def compiletofile(file : String) = {
    val program = read(pwd / file)
    val name = file.substring(0, file.indexOf("."))

    println(s"==== '$file' =====")
    write.over(pwd / s"$name.ll", parsecompile(program))
    println("written to " + s"$name.ll")
}

@main
def compileandrun(file : String) = {
    val name = file.substring(0, file.indexOf("."))

    compiletofile(file)
    os.proc("llc", "-filetype=obj", name ++ ".ll").call()
    os.proc("gcc", name ++ ".o", "-o", name ++ ".bin").call()
    os.proc(os.pwd / (name ++ ".bin")).call(stdout = os.Inherit)
    println(s"done.")
}