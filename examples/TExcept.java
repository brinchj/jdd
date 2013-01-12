package examples;

class TExcept {

    TExcept() {
        System.out.println("before try0");
        try {
	    System.out.println("before try1");
            try {
                System.out.println("inside try1");
            } catch (NullPointerException e) {
                System.out.println("inside catch1-0");
            } finally {
                System.out.println("inside finally1");
            }
            System.out.println("after try1");
        } catch (NullPointerException e) {
	    System.out.println("inside catch0-0");
            try {
                System.out.println("try in catch0-0");
            } finally {
                System.out.println("inside finally0");
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
	new TExcept();
        System.out.println("done");
    }

}
