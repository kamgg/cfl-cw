
.class public factorial.factorial
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

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

   ldc "input n: " 		; '"input n: "'
   invokestatic factorial/factorial/writeStr(Ljava/lang/String;)V
   invokestatic factorial/factorial/read()I
   istore 0 		; n
   ldc 1
   istore 1 		; sum
   ldc 1
   istore 2 		; i
Loop_begin_0:
   iload 2 		; i
   iload 0 		; n
   if_icmpgt loop_end_1
   iload 1 		; sum
   iload 2 		; i
   imul
   istore 1 		; sum
   iload 2 		; i
   ldc 1
   iadd
   istore 2 		; i
   goto Loop_begin_0
loop_end_1:
   ldc "n factorial is " 		; '"n factorial is "'
   invokestatic factorial/factorial/writeStr(Ljava/lang/String;)V
   iload 1 		; sum
   invokestatic factorial/factorial/writeVar(I)V

; COMPILED CODE ENDS
   return

.end method
