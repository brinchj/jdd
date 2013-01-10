package examples;

class TExcept {

    TExcept() {
        System.out.println("before try");
        try {
            System.out.println("inside try");
        } catch (Exception e) {
            System.out.println("inside catch");
        } finally {
            System.out.println("inside finally");
        }
        System.out.println("after try");
    }

    public static void main(String[] args) {
	try {
            new TExcept();
        } catch (Exception e) {
            System.out.println("inside main catch");
        }
        System.out.println("done");
    }

}
