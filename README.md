jdd
===

Compile Main executable:

    jdd > ghc --make Main

Run test-suite:

    jdd > ./Main --test
    Cases: 9  Tried: 9  Errors: 0  Failures: 0
    
Decompile class file:
    
    jdd > javac examples/HelloWorld.java
    jdd > ./Main examples/HelloWorld.class
    package examples;

    class HelloWorld extends java.lang.Object {

        HelloWorld() {
            examples.HelloWorld l0;
            Object s1;
            l0 = this;
            s1 = new Object();
            return;
        }

        public static void main(String[] l0) {
            System.out.println("Hello world!") /* empty assign */ ;
            return;
        }

    }
