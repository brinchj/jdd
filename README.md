jdd
===

    jdd > ghci Test
    *Test> :set -XOverloadedStrings
    *Test> list "test/HelloWorld.class" 
    ["0<init>","1main"]
    *Test> run "test/HelloWorld.class" "1main"

    -- Method bytecode:
    (Nothing, _ <- invoke I_virtual @R_staticField (Class {classPath = "java/lang/System"}) 
      (Desc {descName = "out", descType = "Ljava/io/PrintStream;"})
      MethodSig {methodClass = Class {classPath = "java/io/PrintStream"},
                 methodName = "println", ...,
                 methodParams = [T_object "java/lang/String"],
                 methodResult = T_void}
      [C_string "Hello world!"])
    (Nothing,return)
    --
    
    -- Method code:
    public static void main(String[] l0) {
        System.out.println("Hello world!") /* empty assign */ ;
        return;
    }
    --
