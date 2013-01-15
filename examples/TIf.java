package examples;

class TIf {

    public static int check(int i) {
        int res = 0;
        if (i > 10) {
            res = 4;
        } else {
            res = 3;
        }
	res += i;
	if (res % 2 == 0) {
	    res -= 2;
	}
	if (res % 2 == 1) {
	   if (res + 1 < 10) {
             return res;
           }
           return res+1;
        } else {
           return res/2;
        }
    }

    public static void main(String[] args) {
        System.out.println(TIf.check(-20));
        System.out.println(TIf.check(-19));
        System.out.println(TIf.check(-18));
        System.out.println(TIf.check(-17));
        System.out.println(TIf.check(-16));
        System.out.println(TIf.check(-15));
        System.out.println(TIf.check(-14));
        System.out.println(TIf.check(-13));
        System.out.println(TIf.check(-12));
        System.out.println(TIf.check(-11));
        System.out.println(TIf.check(-10));
        System.out.println(TIf.check(-9));
        System.out.println(TIf.check(-8));
        System.out.println(TIf.check(-7));
        System.out.println(TIf.check(-6));
        System.out.println(TIf.check(-5));
        System.out.println(TIf.check(-4));
        System.out.println(TIf.check(-3));
        System.out.println(TIf.check(-2));
        System.out.println(TIf.check(-1));
        System.out.println(TIf.check(0));
        System.out.println(TIf.check(1));
        System.out.println(TIf.check(2));
        System.out.println(TIf.check(3));
        System.out.println(TIf.check(4));
        System.out.println(TIf.check(5));
        System.out.println(TIf.check(6));
        System.out.println(TIf.check(7));
        System.out.println(TIf.check(8));
        System.out.println(TIf.check(9));
        System.out.println(TIf.check(10));
        System.out.println(TIf.check(11));
        System.out.println(TIf.check(12));
        System.out.println(TIf.check(13));
        System.out.println(TIf.check(14));
        System.out.println(TIf.check(15));
        System.out.println(TIf.check(16));
        System.out.println(TIf.check(17));
        System.out.println(TIf.check(18));
        System.out.println(TIf.check(19));
        System.out.println(TIf.check(20));
    }

}
