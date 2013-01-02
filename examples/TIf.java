package examples;

class TIf {

    public static void main(String[] args) {
        System.out.println(TIf.check(42));
        System.out.println(TIf.check(41));
        System.out.println(TIf.check(43));
        System.out.println(TIf.check(0));
        System.out.println(TIf.check(-1));
        System.out.println(TIf.check(100));
        System.out.println(TIf.check(101));
        System.out.println(TIf.check(400));
    }

    public static int check(int i) {
        if (i > 100) {
            return 42;
        } else {
            return 13;
        }
    }
}
