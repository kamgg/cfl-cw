// Kamal Asmatpoor K1889025, 1807451
// kamal.asmatpoor@kcl.ac.uk

// Imports
import $file.lexer, lexer._
import $file.parser, parser._
import ammonite.ops._

// Compiler headers needed for JVM
// Contains read int and write
val headers = """
.class public XXX.XXX
.super java/lang/Object

.method public static writeVar(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/println(I)V 
    return 
.end method

.method public static writeStr(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method


.method public static read()I 
    .limit locals 10 
    .limit stack 10

    ldc 0 
    istore 1  ; this will hold our final integer 
Label1: 
    getstatic java/lang/System/in Ljava/io/InputStream; 
    invokevirtual java/io/InputStream/read()I 
    istore 2 
    iload 2 
    ldc 10   ; the newline delimiter 
    isub 
    ifeq Label2 
    iload 2 
    ldc 32   ; the space delimiter 
    isub 
    ifeq Label2

    iload 2 
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48 
    isub 
    ldc 10 
    iload 1 
    imul 
    iadd 
    istore 1 
    goto Label1 
Label2: 
    ;when we come here we have our integer computed in local variable 1 
    iload 1 
    ireturn 
.end method
"""

val main_start = """
.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

val main_end = """
; COMPILED CODE ENDS
   return

.end method
"""

// Compiler

// Generate a fresh label
var counter = -1
def Fresh(x: String) = {
    counter += 1
    x ++ "_" ++ counter.toString()
}

// convenient string interpolations 
// for instructions and labels
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def string_interpolations(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

// Environments
type Env = Map[String, Integer]

// Operators to bytecode
def compile_op(op: String) = op match {
    case "+" => i"iadd"
    case "-" => i"isub"
    case "*" => i"imul"
    case "/" => i"idiv"
    case "%" => i"irem"
}


// Arithmetic expression to bytecode
def compile_aexp(a: AExp, env: Env) : String = a match {
    case Num(i) => i"ldc $i"
    case Var(s) => i"iload ${env(s)} \t\t; $s"
    case Aop(a1, op, a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

// Boolean expression to bytecode
def compile_bexp(b: BExp, env: Env, jmp: String) : String = b match {
    case True => ""
    case False => i"goto $jmp"
    case And(x, z) => compile_bexp(x, env, jmp) ++ compile_bexp(z, env, jmp)
    case Or(x, z) => {
        val or_mid = Fresh("Or_mid")
        val or_end = Fresh("Or_end")

        compile_bexp(x, env, or_mid) ++
        i"goto $or_end" ++
        l"$or_mid" ++
        compile_bexp(z, env, jmp) ++
        l"$or_end"
    }
    case Bop(a1,"==", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
    case Bop(a1, "!=", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
    case Bop(a1, "<", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
    case Bop(a1, ">", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp"
    case Bop(a1, "<=", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"
    case Bop(a1, ">=", a2) => 
        compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
}


// Statements to bytecode
def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
    case Skip => ("", env)
    case Assign(x, a) => {
        val index: Integer = env.getOrElse(x, env.keys.size)
        (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
    }
    case If(b, bl1, bl2) => {
        val if_else = Fresh("If_else")
        val if_end = Fresh("If_end")
        val (instrs1, env1) = compile_block(bl1, env)
        val (instrs2, env2) = compile_block(bl2, env1)
        (compile_bexp(b, env, if_else) ++
        instrs1 ++
        i"goto $if_end" ++
        l"$if_else" ++
        instrs2 ++
        l"$if_end", env2)
    }
    case While(b, bl) => {
        val loop_begin = Fresh("Loop_begin")
        val loop_end = Fresh("Loop_end")
        val (instrs1, env1) = compile_block(bl, env)
        (l"$loop_begin" ++
        compile_bexp(b, env, loop_end) ++
        instrs1 ++
        i"goto $loop_begin" ++
        l"$loop_end", env1)
    }
    // CW04 Question 2
    case For(x, from, to, bl) => {
        val loop_begin = Fresh("Loop_begin")
        val loop_end = Fresh("loop_end")
        
        // Get index of variable 'x'
        val index: Integer = env.getOrElse(x, env.keys.size)
        val expression = Bop(Var(x), "<=", to)
        val increment = Assign(x, Aop(Var(x), "+", Num(1)))

        val (instrs1, env1) = compile_block(bl, env + (x -> index))

        (compile_aexp(from, env1) ++
         i"istore $index \t\t; $x" ++
         l"$loop_begin"  ++
         compile_bexp(expression, env1, loop_end) ++
         instrs1 ++
         compile_stmt(increment, env1)._1 ++
         i"goto $loop_begin" ++
         l"$loop_end", env1)
    }
    case WriteVar(x) => (i"iload ${env(x)} \t\t; $x" ++ 
                         i"invokestatic XXX/XXX/writeVar(I)V", env)
    case WriteStr(x) => (i"ldc $x \t\t; '$x'" ++
                         i"invokestatic XXX/XXX/writeStr(Ljava/lang/String;)V", env)
    case Read(x) => {
        // Get index of x if in environment, otherwise set index to size of environment
        val index: Integer = env.getOrElse(x, env.keys.size)
        (i"invokestatic XXX/XXX/read()I" ++
         i"istore $index \t\t; $x", env + (x -> index))
    }
}

// Block to bytecode
def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
    case Nil => ("", env)
    case s::bl => {
        // Recursively build list of instructions
        val (instrs1, env1) = compile_stmt(s, env)
        val (instrs2, env2) = compile_block(bl, env1)
        (instrs1 ++ instrs2, env2)
    }
}

// Main compilation function
def compile(bl: Block, class_name: String) : String = {
    val instructions = compile_block(bl, Map.empty)._1
    (headers ++
     main_start ++
     instructions ++
     main_end).replaceAllLiterally("XXX", class_name)
}

// Parse string then compile and return bytecode 
def parsecompile(program: String, class_name: String) : String =
    compile(parse(program), class_name)

@main
def compilefromfile(file : String) = {
    val program = read(pwd / file)
    val name = file.substring(0, file.indexOf("."))
    println(s"==== '$file' =====")
    println(parsecompile(program, name))
}

@main
def compiletofile(file : String) = {
    val program = read(pwd / file)
    val name = file.substring(0, file.indexOf("."))

    println(s"==== '$file' =====")
    write.over(pwd / s"$name.j", parsecompile(program, name))
    println("written to " + s"$name.j")
}

@main
def compileandrun(file : String) = {
    val program = read(pwd / file)
    val name = file.substring(0, file.indexOf("."))

    println(s"==== '$file' =====")
    write.over(pwd / s"$name.j", parsecompile(program, name))
    println("written to " + s"$name.j")

    os.proc("java", "-jar", "jasmin.jar", s"$name.j").call()
    os.proc("java", s"$name/$name").call(stdout = os.Inherit, stdin = os.Inherit)
}