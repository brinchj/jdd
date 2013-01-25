package examples;

class TExcept {

    TExcept(int i) {
        System.out.println("before try0");
        try {
	    System.out.println("before try1");
            while(true) {
            try {
                if(i == 1) {
                  System.out.println("inside try1");
                  return;
                }
                break;
            } catch (NullPointerException e) {
                if (i == 1) {
                  return;
                }
                System.out.println("inside catch1-0");
            } finally {
                System.out.println("inside finally1");
            }
            }
            System.out.println("after try1");
            throw new NullPointerException();
        } catch (NullPointerException e) {
	    System.out.println("inside catch0-0");
            try {
                System.out.println("try in catch0-0");
            } finally {
                System.out.println("inside finally0-0");
            }
	    System.out.println("after catch0-0");
        } catch (Exception e) {
            System.out.println("inside catch0-1");
        } finally {
            System.out.println("inside finally0");
        }
        System.out.println("after try0");
    }

    public static void main(String[] args) {
        System.out.println("main");
	new TExcept(0);
	new TExcept(1);
        System.out.println("done");
    }

}
