jdd
===

    jdd > ghci Test
    *Test> :set -XOverloadedStrings
    *Test> code <- decompileClass "test/HelloWorld.class"
    *Test> putStrLn code
    package test;

        class HelloWorld extends java.lang.Object {

        void HelloWorld() {
             return;
        }

        public static void main(String[] l0) {
            System.out.println("Hello world!");
            return;
        }

    }
