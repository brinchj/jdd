package examples;

class TExcept {

    TExcept() {
        System.out.println("before try");
        try {
            System.out.println("inside try");
        } catch (NullPointerException e) {
            System.out.println("inside catch");
        } catch (Exception e) {
            System.out.println("inside catch");
        } finally {
            System.out.println("inside finally");
        }
        System.out.println("after try");
    }

    public static void main(String[] args) {
        System.out.println("main");
	new TExcept();
        System.out.println("done");
    }

}
