package examples;

class TDo {

    public static void main(String[] args) {
      System.out.println(new TDo().fakeLoop());
    }

    public int fakeLoop() {
        int i = 0;
        do {
            i ++;
            if (i > 10) {
                break;
            }
            i ++;
            if (i < 0) {
                continue;
            }
            i = i*2;
        } while (true);
        return i;
    }
}
