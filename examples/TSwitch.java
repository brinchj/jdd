package examples;

class TSwitch {

    public static void main(String[] args) {
        int i;
        for(i = 0; i < 50; i++) {
            System.out.println(new TSwitch().run0(i));
            System.out.println(new TSwitch().run1(i));
            System.out.println(new TSwitch().run2(i));
            System.out.println(new TSwitch().run3(i));
            System.out.println(new TSwitch().run4(i));
        }
    }

    public int run0(int n) {
        int i = 0;
        switch(n) {
        case 42:
            if(i < 12) {
              i = 42;
            }
            break;
        case 21:
            i = 21;
            break;
        }
        return i;
    }

    public int run1(int n) {
        int i = 0;
        switch(n) {
        case 42:
            i = 42;
        case 21:
            i = 21;
        }
        return i;
    }

    public int run2(int n) {
        int i = 0;
        switch(n) {
        case 42:
            i = 42;
            break;
        case 21:
            i = 21;
        default:
            i = 1;
        }
        return i;
    }

    public int run3(int n) {
        int i = 0;
        switch(n) {
        case 42:
            i = 42;
        case 21:
            i = 21;
        default:
            i = 1;
        }
        return i;
    }

    public int run4(int n) {
        int i = 0;
        switch(n) {
        case 42:
            if(i != 42) {
                break;
            }
            i = 42;
        case 21:
            i = 21;
        default:
            i = 1;
        }
        return i;
    }

}
