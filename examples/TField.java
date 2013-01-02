package examples;

class TField {

  public int foo;

  public static void main(String[] args) {
    TField jf = new TField(42);
    System.out.println(jf.getFoo());
  }

  TField(int i) {
    TField t;
    t = this;
    t.foo = i;
    return;
  }

  public int getFoo() {
    return this.foo;
  }

}
