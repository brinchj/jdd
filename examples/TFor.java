package examples;

class TFor {

    public static void main(String[] args) {
        int i = 0;
        int j = i;
        for(i = 0; i < 123; i++) {
            j += i;
        }
        System.out.println(j);
    }

}
