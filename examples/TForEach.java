package examples;

class TForEach {
    public static void main(String[] args){
      System.out.println(loop()[0]);
    }

    public static int[] loop() {
        int[] list = new int[10];
        for(int i : list) {
            list[0] += i;
        }
        return list;
    }
}
