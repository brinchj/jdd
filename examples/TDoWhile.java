package examples;

class TDoWhile {

    public static void main(String[] args) {
        System.out.println(new TDoWhile().loop());
    }

    public int loop() {
        int i = 0;
        do{
            i ++;
        } while(i < 11);
        return i;
    }

}
