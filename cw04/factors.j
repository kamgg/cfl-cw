
.class public factors.factors
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

   ldc "Input n please" 		; '"Input n please"'
   invokestatic factors/factors/writeStr(Ljava/lang/String;)V
   invokestatic factors/factors/read()I
   istore 0 		; n
   ldc "The factors of n are" 		; '"The factors of n are"'
   invokestatic factors/factors/writeStr(Ljava/lang/String;)V
   ldc 2
   istore 1 		; f
Loop_begin_0:
   iload 0 		; n
   ldc 1
   if_icmpeq Loop_end_1
Loop_begin_2:
   iload 0 		; n
   iload 1 		; f
   idiv
   iload 1 		; f
   imul
   iload 0 		; n
   if_icmpne Loop_end_3
   iload 1 		; f
   invokestatic factors/factors/writeVar(I)V
   iload 0 		; n
   iload 1 		; f
   idiv
   istore 0 		; n
   goto Loop_begin_2
Loop_end_3:
   iload 1 		; f
   ldc 1
   iadd
   istore 1 		; f
   goto Loop_begin_0
Loop_end_1:

; COMPILED CODE ENDS
   return

.end method
